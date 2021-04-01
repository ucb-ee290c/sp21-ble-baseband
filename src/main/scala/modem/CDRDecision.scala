package modem

import chisel3._
import chisel3.util._
import chisel3.experimental.{DataMirror, FixedPoint}
import baseband.BLEBasebandModemParams

class CDRDecision(params: BLEBasebandModemParams) extends Module {
  val io = IO(new Bundle {
      val signal = Flipped(Decoupled(SInt(6.W)))
      val out = Decoupled(UInt(1.W))
  })

  val bandpassF0 = Module( new GenericFIR(FixedPoint(6.W, 0.BP), FixedPoint(19.W, 11.BP),
    FIRCoefficients.GFSKRX_Bandpass_F0.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))) )

  val bandpassF1 = Module( new GenericFIR(FixedPoint(6.W, 0.BP), FixedPoint(19.W, 11.BP),
    FIRCoefficients.GFSKRX_Bandpass_F1.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))) )

  bandpassF0.io.in.bits.data := io.signal.bits.asFixedPoint(0.BP)
  io.signal.ready := bandpassF0.io.in.ready
  bandpassF0.io.in.valid := io.signal.valid

  bandpassF1.io.in.bits.data := io.signal.bits.asFixedPoint(0.BP)
  io.signal.ready := bandpassF1.io.in.ready
  bandpassF1.io.in.valid := io.signal.valid

  val envelopeDetectorF0 = Module( new EnvelopeDetector(8) )
  val envelopeDetectorF1 = Module( new EnvelopeDetector(8) )

  envelopeDetectorF0.io.in.valid := bandpassF0.io.out.valid
  envelopeDetectorF0.io.in.bits := bandpassF0.io.out.bits.data(18, 11).asSInt
  bandpassF0.io.out.ready := envelopeDetectorF0.io.in.ready

  envelopeDetectorF1.io.in.valid := bandpassF1.io.out.valid
  envelopeDetectorF1.io.in.bits := bandpassF1.io.out.bits.data(18, 11).asSInt
  bandpassF1.io.out.ready := envelopeDetectorF1.io.in.ready

  envelopeDetectorF0.io.out.ready := io.out.ready
  envelopeDetectorF1.io.out.ready := io.out.ready

  def risingedge(x: Bool) = x && !RegNext(x)
  val guess = Wire(Bool())
  val cdr = Module(new CDR)
  val beginSampling = Wire(Bool())
  guess := Mux((Cat(0.U(1.W), envelopeDetectorF1.io.out.bits).asSInt() -& Cat(0.U(1.W), envelopeDetectorF0.io.out.bits).asSInt()) > 0.S, 1.B, 0.B)
  val accumulator = Wire(SInt(8.W))
  accumulator := RegNext(Mux(beginSampling, 0.S, accumulator + Mux(guess, 1.S, (-1).S).asSInt()), 0.S(8.W))
  cdr.io.d := guess
  beginSampling := risingedge(cdr.io.clk)
  io.out.valid := beginSampling
  io.out.bits := Mux(accumulator > 0.S, 1.U, 0.U)
}
