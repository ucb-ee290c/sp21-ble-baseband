package baseband

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

import verif._
import  ee290cdma._
import modem._

class ControllerAndBasebandTester(params: BLEBasebandModemParams =  BLEBasebandModemParams(), beatBytes: Int) extends Module {
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new BLEBasebandModemCommand))
    val constants = Output(new BasebandConstants)
    val  controllerDMA = new Bundle {
      val readReq = Decoupled(new EE290CDMAReaderReq(params.paddrBits, params.maxReadSize))
      val readResp = Flipped(Decoupled(new EE290CDMAReaderResp(params.maxReadSize)))
    }
    val basebandDMA = new BasebandDMAIO(params.paddrBits, beatBytes)
    val modem = Flipped(new GFSKModemDigitalIO)
  })

  val controller = Module(new Controller(params, beatBytes))
  io.cmd <> controller.io.cmd
  io.controllerDMA <> controller.io.dma

  io.constants := controller.io.constants

  val baseband = Module(new Baseband(params, beatBytes))
  baseband.io.control <> controller.io.basebandControl
  baseband.io.constants := controller.io.constants
  io.basebandDMA <> baseband.io.dma
  io.modem <> baseband.io.modem
}

class ControllerTest extends AnyFlatSpec with ChiselScalatestTester {
  val tests = 40

  def seqToBinary(in: Seq[Int]): Seq[Int] = {
    in.map(x => String.format("%8s", x.toBinaryString).replaceAll(" ", "0").reverse).mkString("").map(c => c.toString.toInt)
  }

  def seqToWidePackets(beatBytes: Int, seq: Seq[Int]): (Seq[BigInt], Seq[Int]) = {
    var in = seq
    var out = Seq[BigInt]()
    var lengths = Seq[Int]()

    while (in.nonEmpty) {
      val (group, rest) = in.splitAt(beatBytes)
      val bytes = group.padTo(beatBytes, 0)

      var sum = BigInt(0)
      for (i <- 0 until beatBytes) {
        sum = sum + (BigInt(bytes(i)) << (8*i))
      }
      lengths = lengths :+ group.length
      out = out :+ sum
      in = rest
    }
    (out, lengths)
  }

  it should "Execute a TX command" in {
    val beatBytes = 4
    val params = BLEBasebandModemParams()
    test(new ControllerAndBasebandTester(params, beatBytes)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val dmaReadReqDriver = new DecoupledDriverSlave(c.clock, c.io.controllerDMA.readReq, 0)
      val dmaReadReqMonitor = new DecoupledMonitor(c.clock, c.io.controllerDMA.readReq)
      val cmdInDriver = new DecoupledDriverMaster(c.clock, c.io.cmd)
      val dmaDataDriver = new DecoupledDriverMaster(c.clock, c.io.basebandDMA.readData)
      val basebandTXDriver = new DecoupledDriverSlave(c.clock, c.io.modem.tx, 0)
      val basebandTXMonitor = new DecoupledMonitor(c.clock, c.io.modem.tx)

      val pduLengthIn = 18
      val addrIn = 4000 * 4
      val aa = BigInt("8E89BED6", 16)

      val preambleExpected = "01010101".map(c => c.toString.toInt)
      val aaExpected = aa.toInt.toBinaryString.reverse.map(c => c.toString.toInt)

      // Disable whitening
      cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
        new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.CONFIG_CMD,
          _.inst.secondaryInst -> BasebandISA.CONFIG_CHANNEL_INDEX, _.additionalData -> 0.U)
      ))

      // Push a send command
      cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
        new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.SEND_CMD,
          _.inst.data -> pduLengthIn.U, _.additionalData -> addrIn.U)
      ))

      while (dmaReadReqMonitor.monitoredTransactions.isEmpty) {
        c.clock.step()
      }

      val pduLength = dmaReadReqMonitor.monitoredTransactions.head.data.totalBytes.litValue.intValue
      val addr = dmaReadReqMonitor.monitoredTransactions.head.data.addr.litValue.intValue


      assert(pduLength == pduLengthIn)
      assert(addr == addrIn)

      dmaReadReqMonitor.monitoredTransactions.clear

      val inBytes = Seq(scala.util.Random.nextInt(255), pduLength) ++ Seq.tabulate(pduLength - 2)(_ => scala.util.Random.nextInt(255))

      val (inData, inSize) = seqToWidePackets(beatBytes, inBytes)

      dmaDataDriver.push(inData.map(d => new DecoupledTX(UInt((beatBytes * 8).W)).tx(d.U)))

      val expectedOut = preambleExpected ++ aaExpected ++ seqToBinary(inBytes)

      while (basebandTXMonitor.monitoredTransactions.length != expectedOut.length) {
        c.clock.step()
      }

      c.clock.step(100)

      assert(basebandTXMonitor.monitoredTransactions.map(tx => tx.data.litValue()).length == (expectedOut.length + 24)) // Add 24 bit CRC

      basebandTXMonitor.monitoredTransactions
        .map(tx => tx.data.litValue())
        .zip(expectedOut)
        .foreach { case (o, e) => assert(o == e) }
    }
  }

  it should "Execute a debug command" in {
    val beatBytes = 4
    val params = BLEBasebandModemParams()
    test(new ControllerAndBasebandTester(params, beatBytes)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val dmaReadReqDriver = new DecoupledDriverSlave(c.clock, c.io.controllerDMA.readReq, 0)
      val dmaReadReqMonitor = new DecoupledMonitor(c.clock, c.io.controllerDMA.readReq)
      val cmdInDriver = new DecoupledDriverMaster(c.clock, c.io.cmd)
      val dmaDataDriver = new DecoupledDriverMaster(c.clock, c.io.basebandDMA.readData)
      val dmaWriteReqDriver = new DecoupledDriverSlave(c.clock, c.io.basebandDMA.writeReq, 0)
      val dmaWriteReqMonitor = new DecoupledMonitor(c.clock, c.io.basebandDMA.writeReq)

      val pduLengthIn = 18
      val addrInString = s"x${4000}"

      // Disable whitening
      cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
        new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.CONFIG_CMD,
          _.inst.secondaryInst -> BasebandISA.CONFIG_CHANNEL_INDEX, _.additionalData -> 0.U)
      ))

      // Push a debug command with post assembler loopback
      cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
        new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.DEBUG_CMD,
          _.inst.secondaryInst -> 2.U, _.inst.data -> pduLengthIn.U, _.additionalData -> addrInString.U)
      ))

      while (dmaReadReqMonitor.monitoredTransactions.isEmpty) {
        c.clock.step()
      }

      val pduLength = dmaReadReqMonitor.monitoredTransactions.head.data.totalBytes.litValue.intValue
      val addr = dmaReadReqMonitor.monitoredTransactions.head.data.addr.litValue.intValue


      assert(pduLength == pduLengthIn)

      dmaReadReqMonitor.monitoredTransactions.clear

      val inBytes = Seq(scala.util.Random.nextInt(255), pduLength - 2) ++ Seq.tabulate(pduLength - 2)(_ => scala.util.Random.nextInt(255))

      val (inData, inSize) = seqToWidePackets(beatBytes, inBytes)

      dmaDataDriver.push(inData.map(d => new DecoupledTX(UInt((beatBytes * 8).W)).tx(d.U)))

      val expectedBaseAddr = (addrInString.U.litValue + pduLength + beatBytes) & ~(beatBytes-1)

      val expectedOut = inData
        .map(d => d.U)
        .zip(inSize
          .map(s => s.U))
        .zip(inSize
          .scanLeft(0)(_ + _)
          .map(o => (o + expectedBaseAddr).U))
        .map {
          case ((d, s), a) => (new EE290CDMAWriterReq(params.paddrBits, beatBytes))
            .Lit(_.data -> d, _.totalBytes -> s, _.addr -> a)
        }

      while (dmaWriteReqMonitor.monitoredTransactions.length != expectedOut.length) {
        c.clock.step()
      }

      c.clock.step(100)

      assert(dmaWriteReqMonitor.monitoredTransactions.map(tx => tx.data.litValue()).length == expectedOut.length)

      dmaWriteReqMonitor.monitoredTransactions
        .map(t => t.data)
        .zip(expectedOut)
        .foreach {
          case (o, e) =>
            assert(o.data.litValue == e.data.litValue)
            assert(o.totalBytes.litValue == e.totalBytes.litValue)
            assert(o.addr.litValue == e.addr.litValue)
        }
    }
  }
}