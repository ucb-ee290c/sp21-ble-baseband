package modem

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec
import baseband.BLEBasebandModemParams
import chisel3.util._

import scala.collection.immutable.Seq
import scala.util.Random
import verif._

class DCOTestHarness extends Module {
  val io = IO(new Bundle {
    val inc = Input(Bool())
    val dec = Input(Bool())
    val count = Output(UInt(5.W))
  })
  val dco = Module(new CDRDCO)
  dco.io.in.inc := io.inc
  dco.io.in.dec := io.dec
  io.count := Counter(dco.io.out.clk, 20)._1
}

class CDRTestHarness extends Module {
  val io = IO(new Bundle {
    val d = Input(Bool())
    val edge = Output(Bool())
    val symbol = Decoupled(UInt(1.W))
  })
  def risingedge(x: Bool) = x && !RegNext(x)
  val cdr = Module(new FPSCDR)

  val preambleDetector = Module(new PreambleDetector())

  preambleDetector.io.control.firstBit := 1.U // TODO: This should be related to the access address
  preambleDetector.io.in := io.d
  preambleDetector.io.control.threshold := 135.U // TODO: THIS SHOULD BE MMIO
  preambleDetector.io.control.reset := io.edge

  val preambleDetected = RegInit(0.B)
  when (risingedge(preambleDetector.io.detected && io.edge)) {
    preambleDetected := 1.B
  }

  cdr.io.d := io.d
  io.edge := risingedge(cdr.io.clk)

  val accumulator = Wire(SInt(8.W))
  accumulator := RegNext(Mux(io.edge, 0.S, accumulator + Mux(io.d, 1.S, (-1).S).asSInt()), 0.S(8.W))
  io.symbol.valid := io.edge & preambleDetected
  io.symbol.bits := Mux(accumulator > 0.S, 1.U, 0.U)

}

class CDRTest extends AnyFlatSpec with ChiselScalatestTester {
  var lfsr = Seq(1, 0, 0, 0, 0, 0, 0)
  def whiten(b: Int): Int = {
    val last = lfsr.last
    lfsr = lfsr.indices.map { i =>

      if (i == 0) {
        lfsr.last
      } else if (i == 4) {
        lfsr.last ^ lfsr(3)
      } else {
        lfsr(i - 1)
      }
    }
    last ^ b
  }
/*
  it should "Expected DCO behavior" in {
    test(new DCOTestHarness()).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.inc.poke(false.B)
      c.io.dec.poke(false.B)
      c.clock.step(10)
      c.clock.step(10)
      assert(c.io.count.peek().litValue() == 10)
    }
    test(new DCOTestHarness()).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.inc.poke(false.B)
      c.io.dec.poke(false.B)
      c.clock.step(10)
      c.io.inc.poke(true.B)
      c.clock.step(1)
      c.io.dec.poke(true.B)
      c.clock.step(9)
      assert(c.io.count.peek().litValue() == 10)
    }
    test(new DCOTestHarness()).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.inc.poke(false.B)
      c.io.dec.poke(false.B)
      c.clock.step(10)
      c.io.dec.poke(true.B)
      c.clock.step(9)
      assert(c.io.count.peek().litValue() == 9)
    }
    test(new DCOTestHarness()).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.inc.poke(false.B)
      c.io.dec.poke(false.B)
      c.clock.step(10)
      c.io.inc.poke(true.B)
      c.clock.step(10)
      assert(c.io.count.peek().litValue() == 11)
    }
  }
*/
  it should "Shift Pulse" in {
    //for (i <- 0 until 10) {
      test(new CDRTestHarness()).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
        val packet = Seq.tabulate(100) { _ => Random.nextInt(2) }.map {
          whiten(_)
        }
        val outDriver = new DecoupledDriverSlave(c.clock, c.io.symbol)
        val outMonitor = new DecoupledMonitor(c.clock, c.io.symbol)
        val phaseOffset = 10//Random.nextInt(21)
        c.clock.step(phaseOffset) // phase offset
        val input = Seq(1, 0, 1, 0, 1, 0, 1, 0) ++ packet
        input.foreach { b =>
          c.io.d.poke(b.B)
          c.clock.step(20)
        }
        c.clock.step(10)
        val retrieved = outMonitor.monitoredTransactions.map {
          _.data.litValue.toInt
        }
        //print("RUN:", i, "\n")
        print("PACKET:", packet, "\n")
        print("OUTPUT:", retrieved, "\n")
        print("OFFSET:", phaseOffset, "\n")
        assert(retrieved.reverse.zip(packet.reverse).forall { p => p._1 == p._2 })
      }
    //}
  }

  /*
  it should "Generate Waveform" in {
    for (i <- 0 until 100) {
      test(new CDRTestHarness()).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
        val packet = Seq.tabulate(100) { _ => Random.nextInt(2) }.map {
          whiten(_)
        }
        val outDriver = new DecoupledDriverSlave(c.clock, c.io.symbol)
        val outMonitor = new DecoupledMonitor(c.clock, c.io.symbol)
        val phaseOffset = Random.nextInt(21)
        c.clock.step(phaseOffset) // phase offset
        val input = Seq(1, 0, 1, 0, 1, 0, 1, 0) ++ packet
        input.foreach { b =>
          c.io.d.poke(b.B)
          c.clock.step(20)
        }
        c.clock.step(10)
        val retrieved = outMonitor.monitoredTransactions.map {
          _.data.litValue.toInt
        }
        print("RUN:", i, "\n")
        print("PACKET:", packet, "\n")
        print("OUTPUT:", retrieved, "\n")
        print("OFFSET:", phaseOffset, "\n")
        assert(retrieved.reverse.zip(packet.reverse).forall { p => p._1 == p._2 })
      }
    }
  }

   */
}
