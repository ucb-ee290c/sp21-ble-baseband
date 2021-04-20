package modem


import baseband.{BLEBasebandModemParams}
import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._

class HilbertFilterIO(params: BLEBasebandModemParams) extends Bundle {
  val control = Input(new Bundle {
    val operation = Bool()
  })
  val in = new Bundle {
    val i = Flipped(Decoupled(UInt(params.adcBits.W)))
    val q = Flipped(Decoupled(UInt(params.adcBits.W)))
  }
  val out = Decoupled(SInt((params.adcBits + 3).W))
}

class HilbertFilter(params: BLEBasebandModemParams) extends Module {
  var io = IO(new HilbertFilterIO(params))

  // Make the inputs I and Q into signed integers centered around 0.
  val I_scaled = Wire(SInt((params.adcBits + 1).W))
  val Q_scaled = Wire(SInt((params.adcBits + 1).W))
  val midpoint = ((math.pow(2, params.adcBits) - 1) / 2).floor.toInt // compute the appropriate midpoint
  I_scaled := Cat(0.U(1.W), io.in.i.bits).asSInt() - midpoint.S((params.adcBits + 1).W)
  Q_scaled := Cat(0.U(1.W), io.in.q.bits).asSInt() - midpoint.S((params.adcBits + 1).W)

  var coeffs = FIRCoefficients.Hilbert.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))

  // The input I should be synchronized with the middle of the FIR hilbert filter for Q, so it should be delayed.
  val I_delayed = ShiftRegister(I_scaled, coeffs.length / 2 + 1)
  val I_valid_delayed = ShiftRegister(io.in.i.valid, coeffs.length / 2 + 1) // the input valid is synchronized with the input

  var fir = Module(
    new GenericFIR(
      FixedPoint((params.adcBits + 1).W, 0.BP),
      FixedPoint((12 + (params.adcBits + 1)).W, 11.BP),
      coeffs)
  )
  fir.io.in.valid := io.in.q.valid
  fir.io.in.bits.data := Q_scaled.asFixedPoint(0.BP)
  fir.io.out.ready := io.out.ready

  io.out.valid := fir.io.out.valid & I_valid_delayed
  // Depending on the control operation, either subtract the output of the FIR filter, or add it to I
  // TODO: Is this okay?
  io.out.bits := Mux(io.control.operation,
                      I_delayed +& Utility.roundTowardsZero(fir.io.out.bits.data),
                      I_delayed -& Utility.roundTowardsZero(fir.io.out.bits.data))
  // Is the filter ready to take in the inputs?
  io.in.i.ready := fir.io.in.ready
  io.in.q.ready := fir.io.in.ready
}
