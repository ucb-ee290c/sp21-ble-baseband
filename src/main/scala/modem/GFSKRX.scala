package modem

import chisel3._
import chisel3.util._
import chisel3.experimental.{DataMirror, FixedPoint}
import baseband.BLEBasebandModemParams

class GFSKRXControlInputBundle extends Bundle {
  val imageRejectionOp = Bool()
}

class GFSKRXControlOutputBundle extends Bundle {
  val preambleDetected = Bool()
}

class GFSKRXControlIO extends Bundle {
  val in = Input(new GFSKRXControlInputBundle)
  val out = Output(new GFSKRXControlOutputBundle)
}

class GFSKRX(params: BLEBasebandModemParams) extends Module {
  val io = IO(new Bundle {
    val analog = new Bundle {
      val i = Flipped(Decoupled(UInt(params.adcBits.W)))
      val q = Flipped(Decoupled(UInt(params.adcBits.W)))
    }
    val digital = new Bundle {
      val out = Decoupled(UInt(1.W))
    }
    val control = new GFSKRXControlIO
  })

  val imageRejection = Module (new HilbertFilter(params))
  imageRejection.io.in <> io.analog

  val demod = Module(new GFSKDemodulation(params))
  demod.io.signal.bits := imageRejection.io.out.data.bits(6,1).asSInt()
  demod.io.signal.valid := imageRejection.io.out.data.valid
  imageRejection.io.out.data.ready := demod.io.signal.ready

  def detectEdge(x: Bool) = x =/= RegNext(x)
  def risingedge(x: Bool) = x && !RegNext(x)

  val guess = Wire(Bool())
  val cdr = Module(new CDR)
  val beginSampling = Wire(Bool())
  guess := demod.io.guess.bits
  demod.io.guess.ready := 1.B

  val accumulator = Wire(SInt(8.W)) // TODO: THIS WIDTH IS VERY IMPORTANT, THIS VALUE CANNOT OVERFLOW
  accumulator := RegNext(Mux(beginSampling, 0.S, accumulator + Mux(guess, 1.S, (-1).S).asSInt()), 0.S(8.W))
  cdr.io.d :=  guess
  beginSampling := risingedge(cdr.io.clk)//risingedge(ShiftRegister(cdr.io.clk, 5))
  io.digital.out.valid := beginSampling
  io.digital.out.bits := Mux(accumulator > 0.S, 1.U, 0.U)

  val preambleDetector = Module(new PreambleDetector())

  preambleDetector.io.control.firstBit := 1.U // TODO: This should be related to the access address
  preambleDetector.io.in := guess
  preambleDetector.io.control.threshold := 140.U // TODO: THIS SHOULD BE MMIO
  preambleDetector.io.control.reset := beginSampling

  // TODO: This is kind of an edge case, the NEXT bit that will be integrated over is the beginning of the packet
  io.control.out.preambleDetected := beginSampling & preambleDetector.io.detected // TODO: this could be problematic if the match is detected exactly at the end
}