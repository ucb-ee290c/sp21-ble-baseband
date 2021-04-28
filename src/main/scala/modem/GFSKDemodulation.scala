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
    val filterCoeffCommand = Input(Valid(new FIRCoefficientChangeCommand))
  })

  /* Bandpass Filters for 0 and 1 frequencies */
  val bandpassF0 = Module(
    new GenericFIR(
      FixedPoint(inputBitwidth.W, 0.BP),
      FixedPoint((inputBitwidth + 12).W, 11.BP),
      FIRCoefficients.GFSKRX_Bandpass_F0.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))
    )
  )
  bandpassF0.io.coeff.bits := io.filterCoeffCommand.bits.change
  bandpassF0.io.coeff.valid := io.filterCoeffCommand.valid && io.filterCoeffCommand.bits.FIR === FIRCodes.RX_BANDPASS_F0

  val bandpassF1 = Module(
    new GenericFIR(
      FixedPoint(inputBitwidth.W, 0.BP),
      FixedPoint((inputBitwidth + 12).W, 11.BP),
      FIRCoefficients.GFSKRX_Bandpass_F1_ALT.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))
    )
  )
  bandpassF1.io.coeff.bits := io.filterCoeffCommand.bits.change
  bandpassF1.io.coeff.valid := io.filterCoeffCommand.valid && io.filterCoeffCommand.bits.FIR === FIRCodes.RX_BANDPASS_F1

  bandpassF0.io.in.bits.data := io.signal.bits.asFixedPoint(0.BP)
  io.signal.ready := bandpassF0.io.in.ready
  bandpassF0.io.in.valid := io.signal.valid

  bandpassF1.io.in.bits.data := io.signal.bits.asFixedPoint(0.BP)
  io.signal.ready := bandpassF1.io.in.ready
  bandpassF1.io.in.valid := io.signal.valid

  /* Envelope Detectors. */
  val envelopeDetectorF0 = Module( new EnvelopeDetector(bandpassF0.io.out.bits.data.getWidth - bandpassF0.io.out.bits.data.binaryPoint.get) )
  val envelopeDetectorF1 = Module( new EnvelopeDetector(bandpassF1.io.out.bits.data.getWidth - bandpassF1.io.out.bits.data.binaryPoint.get) )

  envelopeDetectorF0.io.filterCoeffCommand.bits := io.filterCoeffCommand.bits.change
  envelopeDetectorF0.io.filterCoeffCommand.valid := io.filterCoeffCommand.valid && io.filterCoeffCommand.bits.FIR === FIRCodes.RX_ENVELOPE_F0

  envelopeDetectorF1.io.filterCoeffCommand.bits := io.filterCoeffCommand.bits.change
  envelopeDetectorF1.io.filterCoeffCommand.valid := io.filterCoeffCommand.valid && io.filterCoeffCommand.bits.FIR === FIRCodes.RX_ENVELOPE_F1

  envelopeDetectorF0.io.in.valid := bandpassF0.io.out.valid
  envelopeDetectorF0.io.in.bits := Utility.roundTowardsZero(bandpassF0.io.out.bits.data)
  bandpassF0.io.out.ready := envelopeDetectorF0.io.in.ready

  envelopeDetectorF1.io.in.valid := bandpassF1.io.out.valid
  envelopeDetectorF1.io.in.bits := Utility.roundTowardsZero(bandpassF1.io.out.bits.data)
  bandpassF1.io.out.ready := envelopeDetectorF1.io.in.ready

  envelopeDetectorF0.io.out.ready := io.guess.ready
  envelopeDetectorF1.io.out.ready := io.guess.ready

  io.guess.valid := envelopeDetectorF0.io.out.valid & envelopeDetectorF1.io.out.valid
  
  val difference = Cat(0.U(1.W), envelopeDetectorF1.io.out.bits).asSInt() -& Cat(0.U(1.W), envelopeDetectorF0.io.out.bits).asSInt()
  io.guess.bits := Mux(difference === 0.S, RegNext(io.guess.bits), Mux(difference > 0.S, 1.B, 0.B))

  /* Connect State Outputs */
  io.state.bandpassF0 := bandpassF0.io.out.bits.data.asUInt()
  io.state.bandpassF1 := bandpassF1.io.out.bits.data.asUInt()
  io.state.envelopeF0 := envelopeDetectorF0.io.out.bits.asUInt()
  io.state.envelopeF1 := envelopeDetectorF1.io.out.bits.asUInt()
}
