package modem

import chisel3._
import chisel3.util._
import chisel3.experimental.{DataMirror, FixedPoint}
import baseband.BLEBasebandModemParams

class GFSKDemodulation(params: BLEBasebandModemParams, inputBitwidth: Int) extends Module {
  val io = IO(new Bundle {
      val signal = Flipped(Decoupled(SInt(inputBitwidth.W)))
      val guess = Decoupled(UInt(1.W))
      val state = Output(new Bundle {
        val bandpassF0 = UInt((inputBitwidth + 12).W)
        val bandpassF1 = UInt((inputBitwidth + 12).W)
        val envelopeF0 = UInt((inputBitwidth + 1).W)
        val envelopeF1 = UInt((inputBitwidth + 1).W)
      })
    val filterCoeffCommand = Flipped(Valid(new FIRCoefficientChangeCommand))
  })


  val bandpassF0Width = FIRCoefficients.GFSKRX_Bandpass_F0.width
  val bandpassF0BP = FIRCoefficients.GFSKRX_Bandpass_F0.binaryPoint
  val bandpassF0Coeffs = RegInit(VecInit(FIRCoefficients.GFSKRX_Bandpass_F0.coefficients.map(c => FixedPoint.fromDouble(c, bandpassF0Width.W, bandpassF0BP.BP))))

  when (io.filterCoeffCommand.fire() && io.filterCoeffCommand.bits.FIR === FIRCodes.RX_BANDPASS_F0) {
    bandpassF0Coeffs(io.filterCoeffCommand.bits.change.coeff) := io.filterCoeffCommand.bits.change.value(bandpassF0Width - 1, 0).asFixedPoint(bandpassF0BP.BP)
  }

  /* Bandpass Filters for 0 and 1 frequencies */
  val bandpassF0 = Module(
    new FixedPointTransposeFIR(
      FixedPoint(inputBitwidth.W, 0.BP),
      FixedPoint((inputBitwidth + bandpassF0Width).W, bandpassF0BP.BP),
      FixedPoint(bandpassF0Width.W, bandpassF0BP.BP),
      FIRCoefficients.GFSKRX_Bandpass_F0.coefficients.length
    )
  )
  bandpassF0.io.coeff := bandpassF0Coeffs

  val bandpassF1Width = FIRCoefficients.GFSKRX_Bandpass_F1_ALT.width
  val bandpassF1BP = FIRCoefficients.GFSKRX_Bandpass_F1_ALT.binaryPoint
  val bandpassF1Coeffs = RegInit(VecInit(FIRCoefficients.GFSKRX_Bandpass_F1_ALT.coefficients.map(c => FixedPoint.fromDouble(c, bandpassF1Width.W, bandpassF1BP.BP))))

  when (io.filterCoeffCommand.fire() && io.filterCoeffCommand.bits.FIR === FIRCodes.RX_BANDPASS_F1) {
    bandpassF1Coeffs(io.filterCoeffCommand.bits.change.coeff) := io.filterCoeffCommand.bits.change.value(bandpassF1Width - 1, 0).asFixedPoint(bandpassF1BP.BP)
  }

  val bandpassF1 = Module(
    new FixedPointTransposeFIR(
      FixedPoint(inputBitwidth.W, 0.BP),
      FixedPoint((inputBitwidth + bandpassF1Width).W, bandpassF1BP.BP),
      FixedPoint(bandpassF1Width.W, bandpassF1BP.BP),
      FIRCoefficients.GFSKRX_Bandpass_F1_ALT.coefficients.length
    )
  )
  bandpassF1.io.coeff := bandpassF1Coeffs

  bandpassF0.io.in.bits := io.signal.bits.asFixedPoint(0.BP)
  io.signal.ready := bandpassF0.io.in.ready
  bandpassF0.io.in.valid := io.signal.valid

  bandpassF1.io.in.bits := io.signal.bits.asFixedPoint(0.BP)
  io.signal.ready := bandpassF1.io.in.ready
  bandpassF1.io.in.valid := io.signal.valid


  val envelopeDetectorWidth = FIRCoefficients.GFSKRX_Envelope_Detector.width
  val envelopeDetectorBP = FIRCoefficients.GFSKRX_Envelope_Detector.binaryPoint
  val envelopeDetectorCoeffs = RegInit(VecInit(FIRCoefficients.GFSKRX_Envelope_Detector.coefficients.map(c => FixedPoint.fromDouble(c, envelopeDetectorWidth.W, envelopeDetectorBP.BP))))

  when (io.filterCoeffCommand.fire() && io.filterCoeffCommand.bits.FIR === FIRCodes.RX_ENVELOPE) {
    envelopeDetectorCoeffs(io.filterCoeffCommand.bits.change.coeff) := io.filterCoeffCommand.bits.change.value(envelopeDetectorWidth - 1, 0).asFixedPoint(envelopeDetectorBP.BP)
  }

  /* Envelope Detectors. */
  val envelopeDetectorF0 = Module( new EnvelopeDetector(bandpassF0.io.out.bits.getWidth - bandpassF0.io.out.bits.binaryPoint.get) )
  val envelopeDetectorF1 = Module( new EnvelopeDetector(bandpassF1.io.out.bits.getWidth - bandpassF1.io.out.bits.binaryPoint.get) )

  envelopeDetectorF0.io.in.valid := bandpassF0.io.out.valid
  envelopeDetectorF0.io.in.bits := Utility.roundTowardsZero(bandpassF0.io.out.bits)
  bandpassF0.io.out.ready := envelopeDetectorF0.io.in.ready
  envelopeDetectorF0.io.coeffs := envelopeDetectorCoeffs

  envelopeDetectorF1.io.in.valid := bandpassF1.io.out.valid
  envelopeDetectorF1.io.in.bits := Utility.roundTowardsZero(bandpassF1.io.out.bits)
  bandpassF1.io.out.ready := envelopeDetectorF1.io.in.ready
  envelopeDetectorF1.io.coeffs := envelopeDetectorCoeffs

  envelopeDetectorF0.io.out.ready := io.guess.ready
  envelopeDetectorF1.io.out.ready := io.guess.ready

  io.guess.valid := envelopeDetectorF0.io.out.valid & envelopeDetectorF1.io.out.valid
  
  val difference = Cat(0.U(1.W), envelopeDetectorF1.io.out.bits).asSInt() -& Cat(0.U(1.W), envelopeDetectorF0.io.out.bits).asSInt()
  io.guess.bits := Mux(difference === 0.S, RegNext(io.guess.bits), Mux(difference > 0.S, 1.B, 0.B))

  /* Connect State Outputs */
  io.state.bandpassF0 := bandpassF0.io.out.bits.asUInt()
  io.state.bandpassF1 := bandpassF1.io.out.bits.asUInt()
  io.state.envelopeF0 := envelopeDetectorF0.io.out.bits.asUInt()
  io.state.envelopeF1 := envelopeDetectorF1.io.out.bits.asUInt()
}
