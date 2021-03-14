package modem


import baseband.{BLEBasebandModemParams, PDAControlInputBundle}
import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import firrtl.ir.Width
import freechips.rocketchip.config.Parameters
import chipyard.example.GenericFIR

class HilbertFilterControlInput extends Bundle {
  val combineOperation = UInt(1.W)
}

class HilbertFilterOutput(bitWidth: Int) extends Bundle {
  val data = Decoupled(SInt(6.W))
}

class HilbertFilterIO(params: BLEBasebandModemParams) extends Bundle {
  val in = new AnalogRXIO(params)
  val out = new HilbertFilterOutput(6)
}



class HilbertFilter(params: BLEBasebandModemParams) extends Module {
  var io = IO(new HilbertFilterIO(params))

  val I_scaled = Wire(SInt((params.adcBits + 1).W))
  val Q_scaled = Wire(SInt((params.adcBits + 1).W))

  var coeffs = Seq[String]("b000000000000", "b000000000000", "b000000000000", "b000000000100", "b000000000000", "b000000010000", "b000000000000", "b000000110100", "b000000000000", "b000010001011", "b000000000000", "b000101011100", "b000000000000", "b010011111000", "b000000000000", "b101100001000", "b000000000000", "b111010100100", "b000000000000", "b111101110101", "b000000000000", "b111111001100", "b000000000000", "b111111110000", "b000000000000", "b111111111100", "b000000000000", "b000000000000", "b000000000000").map(c => c.U.asFixedPoint(11.BP))

  val I_delay = Module (new GenericDelayChain(coeffs.length / 2, SInt((params.adcBits + 1).W)))
  var fir = Module( new GenericFIR(FixedPoint(6.W, 0.BP), FixedPoint(18.W, 11.BP), coeffs) )

  // TODO: might need to add an additional bit in order to make sure that the fixed point value wont be negative
  //io.in.i.data.asFixedPoint(0.BP) // TODO: How does this conversion work? Does this produce an 8 bit FP with the integer component all above the point?
  I_scaled := Cat(0.U(1.W), io.in.i.data).asSInt() - 15.S((params.adcBits +1).W)
  Q_scaled := Cat(0.U(1.W), io.in.q.data).asSInt() - 15.S((params.adcBits +1).W)

  I_delay.io.in.valid :=  io.in.i.valid
  I_delay.io.in.bits := I_scaled
  I_delay.io.out.ready := io.out.data.ready

  fir.io.in.valid := io.in.q.valid
  fir.io.in.bits.data := Q_scaled.asFixedPoint(0.BP)
  fir.io.out.ready := io.out.data.ready
  io.out.data.valid := I_delay.io.out.valid & fir.io.out.valid
  io.out.data.bits := (fir.io.out.bits.data.asSInt() >> fir.io.out.bits.data.binaryPoint.get)(5, 0).asSInt()
}
