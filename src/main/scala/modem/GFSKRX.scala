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
  imageRejection.io.in.i := io.analog.i
  imageRejection.io.in.q := io.analog.q

  val bandpassF0 = Module( new GenericFIR(FixedPoint(6.W, 0.BP), FixedPoint(19.W, 11.BP),
    FIRCoefficients.GFSKRX_Bandpass_F0.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))) )

  val bandpassF1 = Module( new GenericFIR(FixedPoint(6.W, 0.BP), FixedPoint(19.W, 11.BP),
    FIRCoefficients.GFSKRX_Bandpass_F1.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))) )


  io.digital.out.valid := false.B
  io.digital.out.bits := 0.U




}