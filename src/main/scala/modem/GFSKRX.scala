package modem

import chisel3._
import chisel3.util._
import chisel3.experimental.{DataMirror, FixedPoint}
import baseband.BLEBasebandModemParams

class GFSKRXControlInputBundle extends Bundle {
  val enable = Bool()
  val imageRejectionControl = UInt(3.W)
  val accessAddressLSB = UInt(1.W)
  val preambleDetectionThreshold = UInt(8.W)
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
    val state = Output(new Bundle {
      val imageRejectionOut = SInt((params.adcBits + 3).W)
      val bandpassF0 = UInt((params.adcBits + 3 + 12).W)
      val bandpassF1 = UInt((params.adcBits + 3 + 12).W)
      val envelopeF0 = UInt((params.adcBits + 3 + 1).W)
      val envelopeF1 = UInt((params.adcBits + 3 + 1).W)
      val accumulatorCount = SInt(accumulatorWidth.W)
    })
    val filterCoeffCommand = Flipped(Valid(new FIRCoefficientChangeCommand))
  })

  /* Hilbert Filter for digital Image Rejection */
  val imageRejection = Module (new HilbertFilter(params))
  imageRejection.io.filterCoeffCommand := io.filterCoeffCommand

  imageRejection.io.in.i.valid := io.analog.i.valid
  imageRejection.io.in.q.valid := io.analog.q.valid
  imageRejection.io.in.i.bits := io.analog.i.bits
  imageRejection.io.in.q.bits := io.analog.q.bits
  imageRejection.io.control.operation := io.control.in.imageRejectionControl(0).asBool()
  imageRejection.io.control.IonLHS := io.control.in.imageRejectionControl(1).asBool()
  imageRejection.io.control.IonTop := io.control.in.imageRejectionControl(2).asBool()
  io.analog.i.ready := imageRejection.io.in.i.ready
  io.analog.q.ready := imageRejection.io.in.q.ready

  /* GFSK Demodulation from recovered signal */
  val guess = Wire(Bool())
  val demod = Module(new GFSKDemodulation(params, params.adcBits + 3))
  demod.io.signal.bits := imageRejection.io.out.bits
  demod.io.signal.valid := imageRejection.io.out.valid
  imageRejection.io.out.ready := demod.io.signal.ready
  demod.io.guess.ready := io.control.in.enable && io.digital.out.ready
  guess := demod.io.guess.bits
  demod.io.filterCoeffCommand := io.filterCoeffCommand

  /* Clock and Data Recovery */
  val cdr = Module(new FPSCDR)
  val beginSampling = Wire(Bool())
  cdr.io.d :=  guess
  beginSampling := Utility.risingedge(cdr.io.clk)

  /* Temporal Majority Voting for determining each symbol, begins on the rising edge of the `beginSampling` signal from CDR */
  val accumulatorWidth = log2Ceil(params.samplesPerSymbol * 2) + 1 // About twice as big as necessary, just to be safe.
  val accumulator = Wire(SInt(accumulatorWidth.W))
  accumulator := RegNext(Mux(beginSampling, 0.S, accumulator + Mux(guess, 1.S, (-1).S).asSInt()), 0.S(accumulatorWidth.W))
  io.digital.out.valid := beginSampling & io.control.out.preambleDetected
  io.digital.out.bits := Mux(accumulator > 0.S, 1.U, 0.U)

  /* Preamble Detection */
  val preambleDetector = Module(new PreambleDetector())
  val preambleDetected = RegInit(0.B) // TODO: Reset these when needed.
  val preambleValid = RegInit(0.B)
  preambleDetector.io.control.firstBit := io.control.in.accessAddressLSB
  preambleDetector.io.in := guess
  preambleDetector.io.control.threshold := io.control.in.preambleDetectionThreshold
  preambleDetector.io.control.reset := beginSampling

  /* Look for a preamble match between the falling edges of the clock. */
  when (preambleDetector.io.detected) {
    preambleDetected := 1.B
  }
  when (Utility.fallingedge(cdr.io.clk)) {
    preambleValid := preambleDetected
  }
  io.control.out.preambleDetected := preambleValid

  // State IO
  io.state.imageRejectionOut := imageRejection.io.out.bits
  io.state.bandpassF0 := demod.io.state.bandpassF0
  io.state.bandpassF1 := demod.io.state.bandpassF1
  io.state.envelopeF0 := demod.io.state.envelopeF0
  io.state.envelopeF1 := demod.io.state.envelopeF1
  io.state.accumulatorCount := accumulator
}