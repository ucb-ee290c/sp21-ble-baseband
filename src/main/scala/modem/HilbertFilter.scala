package modem


import baseband.{BLEBasebandModemParams}
import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._

class HilbertFilterIO(params: BLEBasebandModemParams) extends Bundle {
  val control = Input(new Bundle {
    val operation = Bool()
  })
  val in = Flipped(Decoupled(new Bundle {
    val i = UInt(params.adcBits.W)
    val q = UInt(params.adcBits.W)
  }))
  val out = Decoupled(SInt(7.W))
}

class HilbertFilter(params: BLEBasebandModemParams) extends Module {
  var io = IO(new HilbertFilterIO(params))

  val I_scaled = Wire(SInt((params.adcBits + 1).W))
  val Q_scaled = Wire(SInt((params.adcBits + 1).W))
  I_scaled := Cat(0.U(1.W), io.in.bits.i).asSInt() - 15.S((params.adcBits + 1).W)
  Q_scaled := Cat(0.U(1.W), io.in.bits.q).asSInt() - 15.S((params.adcBits + 1).W)

  var coeffs = FIRCoefficients.Hilbert.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))

  val I_delay = ShiftRegister(I_scaled, coeffs.length / 2 + 1)
  var fir = Module( new GenericFIR(
                                  FixedPoint((params.adcBits + 1).W, 0.BP),
                                  FixedPoint((12 + (params.adcBits + 1)).W, 11.BP),
                                  coeffs)
  )
  fir.io.in.valid := io.in.valid
  fir.io.in.bits.data := Q_scaled.asFixedPoint(0.BP)
  fir.io.out.ready := io.out.ready
  io.out.valid := fir.io.out.valid
  io.out.bits := I_delay -& (fir.io.out.bits.data.asSInt() >> fir.io.out.bits.data.binaryPoint.get)(5, 0).asSInt()
  io.in.ready := fir.io.in.ready
}
