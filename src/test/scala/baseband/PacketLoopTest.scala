package baseband

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

import verif._

class PacketLoop extends Module {
  val io = IO(new Bundle {
    val assembler = new PAInputIO
    val disassembler = new Bundle {
      val out = new PDAOutputIO
      val control = Flipped(Decoupled(new PDAControlInputBundle))
    }
    val constants = Input(new BasebandConstants)
  })

  val assembler = Module(new PacketAssembler)
  val disassembler = Module(new PacketDisassembler)

  assembler.io.constants := io.constants
  disassembler.io.constants := io.constants

  assembler.io.in <> io.assembler
  io.disassembler.out <> disassembler.io.out
  disassembler.io.in.control <> io.disassembler.control

  assembler.io.out.data <> disassembler.io.in.data
}

class PacketLoopTest extends AnyFlatSpec with ChiselScalatestTester {
  def seqToBinary(in: Seq[Int]): Seq[Int] = {
    in.map(x => String.format("%8s", x.toBinaryString).replaceAll(" ", "0").reverse).mkString("").map(c => c.toString.toInt)
  }

  it should "Pass a circular test without whitening" in {
    test(new PacketLoop).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val paControlDriver = new DecoupledDriverMaster(c.clock, c.io.assembler.control)
      val pdaControlDriver = new DecoupledDriverMaster(c.clock, c.io.disassembler.control)
      val paDMADriver = new DecoupledDriverMaster(c.clock, c.io.assembler.data)
      val pdaDMADriver = new DecoupledDriverSlave(c.clock, c.io.disassembler.out.data, 0) // TODO: randomize?
      val pdaDMAMonitor = new DecoupledMonitor(c.clock, c.io.disassembler.out.data)

      val pduLength = scala.util.Random.nextInt(255)
      val inBytes = Seq(0, pduLength) ++ Seq.tabulate(pduLength)(_ => scala.util.Random.nextInt(255))
      //val pduLength = 4
      //val inBytes = Seq(0, pduLength) ++ Seq.tabulate(pduLength)(i => i)
      val aa = BigInt("8E89BED6", 16)

      c.io.constants.channelIndex.poke("b000000".U)

      paDMADriver.push(inBytes.map(x => (new DecoupledTX(UInt(8.W))).tx(x.U)))
      paControlDriver.push(new DecoupledTX(new PAControlInputBundle).tx((new PAControlInputBundle).Lit(_.aa -> aa.U, _.pduLength -> pduLength.U)))
      pdaControlDriver.push(new DecoupledTX(new PDAControlInputBundle).tx((new PDAControlInputBundle).Lit(_.aa -> aa.U)))

      c.clock.step(pduLength*15)

      val expectedOut = inBytes

      assert(pdaDMAMonitor.monitoredTransactions.map(x => x.data.litValue()).length == expectedOut.length)

      pdaDMAMonitor.monitoredTransactions
        .map(x => x.data.litValue())
        .zip(expectedOut)
        .foreach {case (o, e) => assert(o == e)}
    }
  }

  it should "Pass a circular test with whitening" in {
    test(new PacketLoop).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val paControlDriver = new DecoupledDriverMaster(c.clock, c.io.assembler.control)
      val pdaControlDriver = new DecoupledDriverMaster(c.clock, c.io.disassembler.control)
      val paDMADriver = new DecoupledDriverMaster(c.clock, c.io.assembler.data)
      val pdaDMADriver = new DecoupledDriverSlave(c.clock, c.io.disassembler.out.data, 0) // TODO: randomize?
      val pdaDMAMonitor = new DecoupledMonitor(c.clock, c.io.disassembler.out.data)

      val pduLength = scala.util.Random.nextInt(255)
      val inBytes = Seq(0, pduLength) ++ Seq.tabulate(pduLength)(_ => scala.util.Random.nextInt(255))
      val aa = BigInt("8E89BED6", 16)

      c.io.constants.channelIndex.poke((scala.util.Random.nextInt(62) + 1).U) // Poke random 6 bit value (not 0)

      paDMADriver.push(inBytes.map(x => (new DecoupledTX(UInt(8.W))).tx(x.U)))
      paControlDriver.push(new DecoupledTX(new PAControlInputBundle).tx((new PAControlInputBundle).Lit(_.aa -> aa.U, _.pduLength -> pduLength.U)))
      pdaControlDriver.push(new DecoupledTX(new PDAControlInputBundle).tx((new PDAControlInputBundle).Lit(_.aa -> aa.U)))

      c.clock.step(pduLength*15)

      val expectedOut = inBytes

      assert(pdaDMAMonitor.monitoredTransactions.map(x => x.data.litValue()).length == expectedOut.length)

      pdaDMAMonitor.monitoredTransactions
        .map(x => x.data.litValue())
        .zip(expectedOut)
        .foreach {case (o, e) => assert(o == e)}
    }
  }

  it should "Pass repeated circular tests without whitening" in {
    test(new PacketLoop).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val paControlDriver = new DecoupledDriverMaster(c.clock, c.io.assembler.control)
      val pdaControlDriver = new DecoupledDriverMaster(c.clock, c.io.disassembler.control)
      val paDMADriver = new DecoupledDriverMaster(c.clock, c.io.assembler.data)
      val pdaDMADriver = new DecoupledDriverSlave(c.clock, c.io.disassembler.out.data, 0) // TODO: randomize?
      val pdaDMAMonitor = new DecoupledMonitor(c.clock, c.io.disassembler.out.data)

      for (_ <- 0 until 4) {
        val pduLength = scala.util.Random.nextInt(255)
        val inBytes = Seq(0, pduLength) ++ Seq.tabulate(pduLength)(_ => scala.util.Random.nextInt(255))
        val aa = BigInt("8E89BED6", 16)

        c.io.constants.channelIndex.poke("b000000".U)

        paDMADriver.push(inBytes.map(x => (new DecoupledTX(UInt(8.W))).tx(x.U)))
        paControlDriver.push(new DecoupledTX(new PAControlInputBundle).tx((new PAControlInputBundle).Lit(_.aa -> aa.U, _.pduLength -> pduLength.U)))
        pdaControlDriver.push(new DecoupledTX(new PDAControlInputBundle).tx((new PDAControlInputBundle).Lit(_.aa -> aa.U)))

        c.clock.step(pduLength * 15)

        val expectedOut = inBytes

        assert(pdaDMAMonitor.monitoredTransactions.map(x => x.data.litValue()).length == expectedOut.length)

        pdaDMAMonitor.monitoredTransactions
          .map(x => x.data.litValue())
          .zip(expectedOut)
          .foreach { case (o, e) => assert(o == e) }

        pdaDMAMonitor.monitoredTransactions.clear
      }
    }
  }

  it should "Pass repeated circular tests with whitening" in {
    test(new PacketLoop).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val paControlDriver = new DecoupledDriverMaster(c.clock, c.io.assembler.control)
      val pdaControlDriver = new DecoupledDriverMaster(c.clock, c.io.disassembler.control)
      val paDMADriver = new DecoupledDriverMaster(c.clock, c.io.assembler.data)
      val pdaDMADriver = new DecoupledDriverSlave(c.clock, c.io.disassembler.out.data, 0) // TODO: randomize?
      val pdaDMAMonitor = new DecoupledMonitor(c.clock, c.io.disassembler.out.data)

      for (_ <- 0 until 4) {
        val pduLength = scala.util.Random.nextInt(255)
        val inBytes = Seq(0, pduLength) ++ Seq.tabulate(pduLength)(_ => scala.util.Random.nextInt(255))
        val aa = BigInt("8E89BED6", 16)

        c.io.constants.channelIndex.poke((scala.util.Random.nextInt(62) + 1).U) // Poke random 6 bit value (not 0)

        paDMADriver.push(inBytes.map(x => (new DecoupledTX(UInt(8.W))).tx(x.U)))
        paControlDriver.push(new DecoupledTX(new PAControlInputBundle).tx((new PAControlInputBundle).Lit(_.aa -> aa.U, _.pduLength -> pduLength.U)))
        pdaControlDriver.push(new DecoupledTX(new PDAControlInputBundle).tx((new PDAControlInputBundle).Lit(_.aa -> aa.U)))

        c.clock.step(pduLength * 15)

        val expectedOut = inBytes

        assert(pdaDMAMonitor.monitoredTransactions.map(x => x.data.litValue()).length == expectedOut.length)

        pdaDMAMonitor.monitoredTransactions
          .map(x => x.data.litValue())
          .zip(expectedOut)
          .foreach { case (o, e) => assert(o == e) }

        pdaDMAMonitor.monitoredTransactions.clear
      }
    }
  }
}