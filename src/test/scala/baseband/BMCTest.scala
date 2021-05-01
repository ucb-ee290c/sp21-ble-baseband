package baseband

import breeze.plot._
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import modem.{GFSKModemAnalogIO, GFSKModemLUTCodes, GFSKModemLUTCommand, TestUtility}
import org.scalatest.flatspec.AnyFlatSpec

import verif._
import  ee290cdma._
import scala.collection.immutable.Seq
import scala.util.Random

class BMCTest extends AnyFlatSpec with ChiselScalatestTester {
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

  val tests = 1
  val params = BLEBasebandModemParams()
  val beatBytes = 4

  it should "Pass a full baseband loop without whitening" in {
    test(new BMC(params, beatBytes)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val cmdInDriver = new DecoupledDriverMaster(c.clock, c.io.cmd)
      val dmaReadReqDriver = new DecoupledDriverSlave(c.clock, c.io.dma.readReq, 0)
      val dmaReadReqMonitor = new DecoupledMonitor(c.clock, c.io.dma.readReq)
      val dmaReadRespDriver= new DecoupledDriverMaster(c.clock, c.io.dma.readResp)
      val dmaDataDriver = new DecoupledDriverMaster(c.clock, c.io.dma.readData)
      val dmaDataMonitor = new DecoupledMonitor(c.clock, c.io.dma.readData)
      val dmaWriteReqDriver = new DecoupledDriverSlave(c.clock, c.io.dma.writeReq, 0)
      val dmaWriteReqMonitor = new DecoupledMonitor(c.clock, c.io.dma.writeReq)


      for (i <- 0 until tests) {
        val channelIndex = 0

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.CONFIG_CMD,
            _.inst.secondaryInst -> BasebandISA.CONFIG_CHANNEL_INDEX,
            _.additionalData -> channelIndex.U)
        ))

        val pduLengthIn = scala.util.Random.nextInt(256) + 2
        val addrInString = s"x${scala.util.Random.nextInt(1600)}0"

        println(s"Test ${i}:\t pduLength ${pduLengthIn},\t addr 0${addrInString}")

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
        assert(addr == addrInString.U.litValue.intValue)

        dmaReadReqMonitor.monitoredTransactions.clear

        val inBytes = Seq(scala.util.Random.nextInt(255), pduLength - 2) ++
          Seq.tabulate(pduLength - 2)(_ => scala.util.Random.nextInt(255))

        val (inData, inSize) = seqToWidePackets(beatBytes, inBytes)

        dmaDataDriver.push(inData.map(d => new DecoupledTX(UInt((beatBytes * 8).W)).tx(d.U)))

        val expectedBaseAddr = (addrInString.U.litValue + pduLength + beatBytes) & ~(beatBytes - 1)

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

        while(dmaDataMonitor.monitoredTransactions.length != inData.length) {
          c.clock.step()
        }

        dmaDataMonitor.monitoredTransactions.clear

        dmaReadRespDriver.push(new DecoupledTX(new EE290CDMAReaderResp(params.maxReadSize))
          .tx((new EE290CDMAReaderResp(params.maxReadSize))
            .Lit(_.bytesRead -> pduLengthIn.U)))

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

        dmaWriteReqMonitor.monitoredTransactions.clear
      }
    }
  }

  it should "Pass a full baseband loop with random channel index" in {
    test(new BMC(params, beatBytes)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val cmdInDriver = new DecoupledDriverMaster(c.clock, c.io.cmd)
      val dmaReadReqDriver = new DecoupledDriverSlave(c.clock, c.io.dma.readReq, 0)
      val dmaReadReqMonitor = new DecoupledMonitor(c.clock, c.io.dma.readReq)
      val dmaReadRespDriver = new DecoupledDriverMaster(c.clock, c.io.dma.readResp)
      val dmaDataDriver = new DecoupledDriverMaster(c.clock, c.io.dma.readData)
      val dmaDataMonitor = new DecoupledMonitor(c.clock, c.io.dma.readData)
      val dmaWriteReqDriver = new DecoupledDriverSlave(c.clock, c.io.dma.writeReq, 0)
      val dmaWriteReqMonitor = new DecoupledMonitor(c.clock, c.io.dma.writeReq)

      for (i <- 0 until tests) {
        val channelIndex = scala.util.Random.nextInt(63)

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.CONFIG_CMD,
            _.inst.secondaryInst -> BasebandISA.CONFIG_CHANNEL_INDEX,
            _.additionalData -> channelIndex.U)
        ))

        val pduLengthIn = scala.util.Random.nextInt(256) + 2
        val addrInString = s"x${scala.util.Random.nextInt(1600)}0"

        println(s"Test ${i}:\t pduLength ${pduLengthIn},\t addr 0${addrInString}")

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
        assert(addr == addrInString.U.litValue.intValue)

        dmaReadReqMonitor.monitoredTransactions.clear

        val inBytes = Seq(scala.util.Random.nextInt(255), pduLength - 2) ++
          Seq.tabulate(pduLength - 2)(_ => scala.util.Random.nextInt(255))

        val (inData, inSize) = seqToWidePackets(beatBytes, inBytes)

        dmaDataDriver.push(inData.map(d => new DecoupledTX(UInt((beatBytes * 8).W)).tx(d.U)))

        val expectedBaseAddr = (addrInString.U.litValue + pduLength + beatBytes) & ~(beatBytes - 1)

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

        while (dmaDataMonitor.monitoredTransactions.length != inData.length) {
          c.clock.step()
        }

        dmaDataMonitor.monitoredTransactions.clear

        dmaReadRespDriver.push(new DecoupledTX(new EE290CDMAReaderResp(params.maxReadSize))
          .tx((new EE290CDMAReaderResp(params.maxReadSize))
            .Lit(_.bytesRead -> pduLengthIn.U)))

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

        dmaWriteReqMonitor.monitoredTransactions.clear
      }
    }
  }

  it should "Output proper GFSK codes on a random channel index" in { // Note: messages shortened such that graphs are viewable
    test(new BMC(params, beatBytes)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val cmdInDriver = new DecoupledDriverMaster(c.clock, c.io.cmd)
      val dmaReadReqDriver = new DecoupledDriverSlave(c.clock, c.io.dma.readReq, 0)
      val dmaReadReqMonitor = new DecoupledMonitor(c.clock, c.io.dma.readReq)
      val dmaReadRespDriver = new DecoupledDriverMaster(c.clock, c.io.dma.readResp)
      val dmaDataDriver = new DecoupledDriverMaster(c.clock, c.io.dma.readData)
      val dmaDataMonitor = new DecoupledMonitor(c.clock, c.io.dma.readData)
      val dmaWriteReqDriver = new DecoupledDriverSlave(c.clock, c.io.dma.writeReq, 0)
      val dmaWriteReqMonitor = new DecoupledMonitor(c.clock, c.io.dma.writeReq)
      val lutCmdInDriver = new DecoupledDriverMaster(c.clock, c.io.lutCmd)


      val analogOutputs = new scala.collection.mutable.Queue[GFSKModemAnalogIO]()
      fork.withRegion(Monitor) { // Record analog data
          while(true) {
            analogOutputs += c.io.analog.data.peek()
            c.clock.step()
          }
      }

      // Program LOFSK LUT with 1:1 mapping of adddress -> value
      lutCmdInDriver.push(Seq.tabulate(64)(i => new DecoupledTX(new GFSKModemLUTCommand()).tx(
        new GFSKModemLUTCommand().Lit(_.lut -> GFSKModemLUTCodes.LOFSK, _.address -> i.U, _.value -> i.U))
      ))

      c.clock.step(64)

      for (i <- 0 until tests) {
        // Set Random Channel Index
        val channelIndex = scala.util.Random.nextInt(63)

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.CONFIG_CMD,
            _.inst.secondaryInst -> BasebandISA.CONFIG_CHANNEL_INDEX,
            _.additionalData -> channelIndex.U)
        ))

        val pduLengthIn = scala.util.Random.nextInt(10) + 2
        val addrInString = s"x${scala.util.Random.nextInt(1600)}0"

        println(s"Test ${i}:\t pduLength ${pduLengthIn},\t addr 0${addrInString}")

        // Push a send command
        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.SEND_CMD,
            _.inst.data -> pduLengthIn.U, _.additionalData -> addrInString.U)
        ))

        while (dmaReadReqMonitor.monitoredTransactions.isEmpty) {
          c.clock.step()
        }

        val pduLength = dmaReadReqMonitor.monitoredTransactions.head.data.totalBytes.litValue.intValue
        val addr = dmaReadReqMonitor.monitoredTransactions.head.data.addr.litValue.intValue


        assert(pduLength == pduLengthIn)
        assert(addr == addrInString.U.litValue.intValue)

        dmaReadReqMonitor.monitoredTransactions.clear

        val inBytes = Seq(scala.util.Random.nextInt(255), pduLength - 2) ++
          Seq.tabulate(pduLength - 2)(_ => scala.util.Random.nextInt(255))

        val (inData, _) = seqToWidePackets(beatBytes, inBytes)

        dmaDataDriver.push(inData.map(d => new DecoupledTX(UInt((beatBytes * 8).W)).tx(d.U)))


        while (dmaDataMonitor.monitoredTransactions.length != inData.length) {
          c.clock.step()
        }

        dmaDataMonitor.monitoredTransactions.clear

        dmaReadRespDriver.push(new DecoupledTX(new EE290CDMAReaderResp(params.maxReadSize))
          .tx((new EE290CDMAReaderResp(params.maxReadSize))
            .Lit(_.bytesRead -> pduLengthIn.U)))


        while (!c.io.interrupt.txFinish.peek().litToBoolean) {
          c.clock.step()
        }

        c.clock.step(50)

        val signedData = analogOutputs.map(d => {
          val i = d.tx.loFSK.litValue().toInt
          ((i & 31) - (i & 32))
        })

        val f = Figure()
        val p = f.subplot(0)
        p += plot(Seq.tabulate(signedData.size)(i => i), signedData, colorcode = "r")
      }
    }
  }

  it should "Properly receive data with random channel index" in {
    test(new BMC(params, beatBytes)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val cmdInDriver = new DecoupledDriverMaster(c.clock, c.io.cmd)
      val dmaWriteReqDriver = new DecoupledDriverSlave(c.clock, c.io.dma.writeReq, 0)
      val dmaWriteReqMonitor = new DecoupledMonitor(c.clock, c.io.dma.writeReq)

      // Set the appropriate tuning parameters
      c.io.tuning.control.imageRejectionControl.poke(0.U)
      c.io.tuning.control.preambleDetectionThreshold.poke(140.U)
      c.io.firCmd.valid.poke(0.B)
      c.io.firCmd.bits.FIR.poke(0.U)
      c.io.firCmd.bits.change.coeff.poke(0.U)
      c.io.firCmd.bits.change.value.poke(0.U)
      for (i <- 0 until tests) {
        val channelIndex = 0
        val accessAddress = scala.util.Random.nextInt.abs
        val crcSeed = s"x555555"

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.CONFIG_CMD,
            _.inst.secondaryInst -> BasebandISA.CONFIG_CHANNEL_INDEX,
            _.additionalData -> channelIndex.U)
        ))
        c.clock.step()

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.CONFIG_CMD,
            _.inst.secondaryInst -> BasebandISA.CONFIG_ACCESS_ADDRESS,
            _.additionalData -> accessAddress.U(32.W))
        ))
        c.clock.step()

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.CONFIG_CMD,
            _.inst.secondaryInst -> BasebandISA.CONFIG_CRC_SEED,
            _.additionalData -> crcSeed.U)
        ))
        c.clock.step()

        val addrInString = s"x${scala.util.Random.nextInt(1600)}0"

        println(s"Test: \t addr 0${addrInString}")

        // Push a debug command with post assembler loopback
        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.RECEIVE_START_CMD,
            _.inst.secondaryInst -> 0.U, _.inst.data -> 0.U, _.additionalData -> addrInString.U)
        ))
        c.clock.step()
        val length = 50
        val (packet, pdu) = TestUtility.packet(accessAddress, length)
        val bits = Seq(0,0,0,0,0,0) ++ packet ++ Seq.tabulate(10){_ => 0}
        val input = TestUtility.testWaveform(bits)

        for (s <- input) {
          c.io.analog.data.rx.i.data.poke(s._1.U(8.W))
          c.io.analog.data.rx.q.data.poke(s._2.U(8.W))
          c.clock.step()
        }

        val expectedBaseAddr = (addrInString.U.litValue + (length + 2) + beatBytes) & ~(beatBytes - 1)
        var outputBits = Seq[BigInt]()
        var outputLength = 0
        dmaWriteReqMonitor.monitoredTransactions
          .map(t => t.data)
          .foreach { o =>
              //assert(o.addr.litValue == expectedBaseAddr + outputLength)
            // TODO: MAKE SURE THE ADDRESSES MATCH
              outputLength = outputLength + o.totalBytes.litValue.toInt
              outputBits = outputBits ++ Seq.tabulate(o.totalBytes.litValue.toInt * 8) {i => (o.data.litValue >> i) & 0x1}
          }
        println("Expected: ", pdu)
        println("Recieved: ", outputBits)
        assert(length + 2 == outputLength)
        assert(pdu == outputBits)
      }
    }
  }

  it should "Recieve two packets after re-entering RX" in {
    test(new BMC(params, beatBytes)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val cmdInDriver = new DecoupledDriverMaster(c.clock, c.io.cmd)
      val dmaWriteReqDriver = new DecoupledDriverSlave(c.clock, c.io.dma.writeReq, 0)
      val dmaWriteReqMonitor = new DecoupledMonitor(c.clock, c.io.dma.writeReq)

      // Set the appropriate tuning parameters
      c.io.tuning.control.imageRejectionControl.poke(0.U)
      c.io.tuning.control.preambleDetectionThreshold.poke(140.U)
      c.io.firCmd.valid.poke(0.B)
      c.io.firCmd.bits.FIR.poke(0.U)
      c.io.firCmd.bits.change.coeff.poke(0.U)
      c.io.firCmd.bits.change.value.poke(0.U)
      for (i <- 0 until tests) {
        val channelIndex = 0
        val accessAddress = scala.util.Random.nextInt.abs
        val crcSeed = s"x555555"

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.CONFIG_CMD,
            _.inst.secondaryInst -> BasebandISA.CONFIG_CHANNEL_INDEX,
            _.additionalData -> channelIndex.U)
        ))
        c.clock.step()

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.CONFIG_CMD,
            _.inst.secondaryInst -> BasebandISA.CONFIG_ACCESS_ADDRESS,
            _.additionalData -> accessAddress.U(32.W))
        ))
        c.clock.step()

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.CONFIG_CMD,
            _.inst.secondaryInst -> BasebandISA.CONFIG_CRC_SEED,
            _.additionalData -> crcSeed.U)
        ))
        c.clock.step()

        val addrInString = s"x${scala.util.Random.nextInt(1600)}0"
        val addrInString2 = s"x${scala.util.Random.nextInt(1600)}0"

        println(s"Test: \t addr 0${addrInString}, addr2 0${addrInString2}")

        // Push a debug command with post assembler loopback
        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.RECEIVE_START_CMD,
            _.inst.secondaryInst -> 0.U, _.inst.data -> 0.U, _.additionalData -> addrInString.U)
        ))
        c.clock.step()
        val length = 50
        val (packet, pdu) = TestUtility.packet(accessAddress, length)
        val bits = Seq(0,0,0,0,0,0) ++ packet ++ Seq.tabulate(10){_ => 0}
        var input = TestUtility.testWaveform(bits)

        for (s <- input) {
          c.io.analog.data.rx.i.data.poke(s._1.U(8.W))
          c.io.analog.data.rx.q.data.poke(s._2.U(8.W))
          c.clock.step()
        }

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.RECEIVE_EXIT_CMD,
            _.inst.secondaryInst -> 0.U, _.inst.data -> 0.U, _.additionalData -> addrInString.U)
        ))

        c.clock.step()
        val (packet2, pdu2) = TestUtility.packet(accessAddress, length)
        val bits2 = Seq.tabulate(50){_ => 0} ++ packet ++ Seq.tabulate(10){_ => 0}
        input = TestUtility.testWaveform(bits2)

        c.clock.step(50)

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.RECEIVE_START_CMD,
            _.inst.secondaryInst -> 0.U, _.inst.data -> 0.U, _.additionalData -> addrInString2.U)
        ))

        for (s <- input) {
          c.io.analog.data.rx.i.data.poke(s._1.U(8.W))
          c.io.analog.data.rx.q.data.poke(s._2.U(8.W))
          c.clock.step()
        }

        val expectedBaseAddr = (addrInString.U.litValue + (length + 2) + beatBytes) & ~(beatBytes - 1)
        var outputBits = Seq[BigInt]()
        var outputLength = 0
        dmaWriteReqMonitor.monitoredTransactions
          .map(t => t.data)
          .foreach { o =>
            print("ADDRESS: ",o.addr.litValue)
            // TODO: MAKE SURE THE ADDRESSES MATCH
            outputLength = outputLength + o.totalBytes.litValue.toInt
            outputBits = outputBits ++ Seq.tabulate(o.totalBytes.litValue.toInt * 8) {i => (o.data.litValue >> i) & 0x1}
          }
        println("Expected: ", pdu)
        println("Recieved: ", outputBits)
        assert(length + 2 == outputLength)
        assert(pdu == outputBits)
      }
    }
  }

  it should "Exit RX Mode without complications" in {
    test(new BMC(params, beatBytes)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val cmdInDriver = new DecoupledDriverMaster(c.clock, c.io.cmd)
      val dmaWriteReqDriver = new DecoupledDriverSlave(c.clock, c.io.dma.writeReq, 0)
      val dmaWriteReqMonitor = new DecoupledMonitor(c.clock, c.io.dma.writeReq)

      // Set the appropriate tuning parameters
      c.io.tuning.control.imageRejectionControl.poke(0.U)
      c.io.tuning.control.preambleDetectionThreshold.poke(140.U)
      for (i <- 0 until tests) {
        val channelIndex = 0
        val accessAddress = scala.util.Random.nextInt.abs
        val crcSeed = s"x555555"

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.CONFIG_CMD,
            _.inst.secondaryInst -> BasebandISA.CONFIG_CHANNEL_INDEX,
            _.additionalData -> channelIndex.U)
        ))
        c.clock.step()

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.CONFIG_CMD,
            _.inst.secondaryInst -> BasebandISA.CONFIG_ACCESS_ADDRESS,
            _.additionalData -> accessAddress.U(32.W))
        ))
        c.clock.step()

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.CONFIG_CMD,
            _.inst.secondaryInst -> BasebandISA.CONFIG_CRC_SEED,
            _.additionalData -> crcSeed.U)
        ))
        c.clock.step()

        val addrInString = s"x${scala.util.Random.nextInt(1600)}0"

        println(s"Test: \t addr 0${addrInString}")

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.RECEIVE_START_CMD,
            _.inst.secondaryInst -> 0.U, _.inst.data -> 0.U, _.additionalData -> addrInString.U)
        ))
        c.clock.step()

        val length = 4
        val (packet, pdu) = TestUtility.packet(accessAddress, length)
        val bits = Seq(0,0,0,0,0,0) ++ packet ++ Seq.tabulate(10){_ => 0}
        val input = TestUtility.testWaveform(bits)

        val terminatePoint = scala.util.Random.nextInt(input.length)
        println(s"Terminate point ${terminatePoint}")

        for (s <- input.take(terminatePoint)) {
          c.io.analog.data.rx.i.data.poke(s._1.U(8.W))
          c.io.analog.data.rx.q.data.poke(s._1.U(8.W))
          c.clock.step()
        }

        println("Trying to push mid-run RX exit")

        cmdInDriver.push(new DecoupledTX(new BLEBasebandModemCommand()).tx(
          new BLEBasebandModemCommand().Lit(_.inst.primaryInst -> BasebandISA.RECEIVE_EXIT_CMD,
            _.inst.secondaryInst -> 0.U, _.inst.data -> 0.U, _.additionalData -> addrInString.U)
        ))

        c.clock.step(50)

      }
    }
  }
}