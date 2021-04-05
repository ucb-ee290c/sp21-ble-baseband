package modem

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec
import baseband.BLEBasebandModemParams
import chisel3.util.{Counter, ShiftRegister}

import scala.collection.immutable.Seq

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
  })
  def detectEdge(x: Bool) = x =/= RegNext(x)

  val shiftClk = Wire(Bool())

  val cdr = Module(new CDR)
  cdr.io.d := io.d
  shiftClk := ShiftRegister(cdr.io.clk, 10)
  io.edge := detectEdge(shiftClk)
}

class CDRTest extends AnyFlatSpec with ChiselScalatestTester {
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
  it should "Generate Waveform" in {
    test(new CDRTestHarness()).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val input = Seq.tabulate(50){_ % 2 == 0}
      c.clock.step(10 + 15)
      input.foreach { b =>
        c.io.d.poke(b.B)
        c.clock.step(20)
      }
    }
  }
}
