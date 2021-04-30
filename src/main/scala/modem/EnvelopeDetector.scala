package modem

import chisel3._
import chisel3.util._
import chisel3.experimental.FixedPoint

class EnvelopeDetector(bitWidth: Int) extends Module {
  val coeffWidth = FIRCoefficients.GFSKRX_Envelope_Detector.width
  val coeffBP = FIRCoefficients.GFSKRX_Envelope_Detector.binaryPoint

  val io = IO(new Bundle {
    val in = Flipped(Decoupled(SInt(bitWidth.W)))
    val out = Decoupled(UInt((bitWidth + 3).W)) // TODO: Determine width of sum output
    val coeffs = Input(Vec(FIRCoefficients.GFSKRX_Envelope_Detector.coefficients.length, FixedPoint(coeffWidth.W, coeffBP.BP)))
  })

  val cyclesPerSymbol = 20 // TODO: Make this a MMIO setting

  // TODO: Check the output width
  val lowpass = Module(
    new FixedPointTransposeFIR(
      FixedPoint((bitWidth + 1).W, 0.BP),
      FixedPoint((bitWidth + 1 + coeffWidth).W, coeffBP.BP),
      FixedPoint(coeffWidth.W, coeffBP.BP),
      FIRCoefficients.GFSKRX_Envelope_Detector.coefficients.length
    )
  )
  lowpass.io.coeff := io.coeffs

  lowpass.io.in.valid := io.in.valid
  lowpass.io.in.bits := Cat(0.U(1.W), io.in.bits.abs()).asFixedPoint(0.BP) // Take absolute value and ensure output as fixed point is interpreted properly
  io.in.ready := lowpass.io.in.ready

  lowpass.io.out.ready := 1.B
  io.out.valid := lowpass.io.out.valid
  io.out.bits := lowpass.io.out.bits.asUInt()(lowpass.io.out.bits.getWidth - 1, coeffBP)
}
