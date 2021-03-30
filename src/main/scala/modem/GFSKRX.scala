package modem

import chisel3._
import chisel3.util._
import chisel3.experimental.{DataMirror, FixedPoint}
import baseband.BLEBasebandModemParams

class GFSKRX(params: BLEBasebandModemParams) extends Module {
  val io = IO(new Bundle {
    val analog = new Bundle {
      val i = Flipped(Decoupled(UInt(params.adcBits.W)))
      val q = Flipped(Decoupled(UInt(params.adcBits.W)))
    }
    val digital = new Bundle {
      val out = Decoupled(UInt(1.W))
    }
  })

  val imageRejection = Module (new HilbertFilter(params))
  imageRejection.io.in <> io.analog

  val bandpassF0 = Module( new GenericFIR(FixedPoint(6.W, 0.BP), FixedPoint(19.W, 11.BP),
    FIRCoefficients.GFSKRX_Bandpass_F0.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))) )

  val bandpassF1 = Module( new GenericFIR(FixedPoint(6.W, 0.BP), FixedPoint(19.W, 11.BP),
    FIRCoefficients.GFSKRX_Bandpass_F1.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))) )

  bandpassF0.io.in.bits.data := imageRejection.io.out.data.bits(6, 1).asFixedPoint(0.BP)
  imageRejection.io.out.data.ready := bandpassF0.io.in.ready
  bandpassF0.io.in.valid := imageRejection.io.out.data.valid

  bandpassF1.io.in.bits.data := imageRejection.io.out.data.bits(6, 1).asFixedPoint(0.BP)
  imageRejection.io.out.data.ready := bandpassF1.io.in.ready
  bandpassF1.io.in.valid := imageRejection.io.out.data.valid

  val envelopeDetectorF0 = Module( new EnvelopeDetector(8) )
  val envelopeDetectorF1 = Module( new EnvelopeDetector(8) )

  envelopeDetectorF0.io.in.valid := bandpassF0.io.out.valid
  envelopeDetectorF0.io.in.bits := bandpassF0.io.out.bits.data(18, 11).asSInt
  bandpassF0.io.out.ready := envelopeDetectorF0.io.in.ready

  envelopeDetectorF1.io.in.valid := bandpassF1.io.out.valid
  envelopeDetectorF1.io.in.bits := bandpassF1.io.out.bits.data(18, 11).asSInt
  bandpassF1.io.out.ready := envelopeDetectorF1.io.in.ready

  envelopeDetectorF0.io.out.ready := io.digital.out.ready
  envelopeDetectorF1.io.out.ready := io.digital.out.ready

  io.digital.out.bits := Mux((Cat(0.U(1.W), envelopeDetectorF1.io.out.bits).asSInt() -& Cat(0.U(1.W), envelopeDetectorF0.io.out.bits).asSInt() > 0.S), 1.U, 0.U)

  io.digital.out.valid := envelopeDetectorF0.io.out.valid & envelopeDetectorF1.io.out.valid
}