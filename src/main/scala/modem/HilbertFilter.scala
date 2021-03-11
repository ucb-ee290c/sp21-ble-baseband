package modem


import baseband.{BLEBasebandModemParams, PDAControlInputBundle}
import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import firrtl.ir.Width
import freechips.rocketchip.config.Parameters
//import chipyard.example.GenericFIR

class HilbertFilterControlInput extends Bundle {
  val combineOperation = UInt(1.W)
}

class HilbertFilterOutput(bitWidth: Int) extends Bundle {
  val data = Decoupled(UInt(bitWidth.W))
}

class HilbertFilterIO(params: BLEBasebandModemParams) extends Bundle {
  val in = new AnalogRXIO(params)
  val out = new HilbertFilterOutput(params.adcBits + 2)
}



class HilbertFilter(params: BLEBasebandModemParams) extends Module {
  var io = IO(new HilbertFilterIO(params))

  val I_scaled = Wire(SInt((params.adcBits + 1).W))
  val Q_scaled = Wire(SInt((params.adcBits + 1).W))

  var coeffs = Seq[Double](0.0,
  0.0,
  0.0,
  0.002,
  0.0,
  0.008,
  0.0,
  0.026,
  0.0,
  0.068,
  0.0,
  0.17,
  0.0,
  0.6212,
  0.0,
  -0.6212,
  0.0,
  -0.17,
  0.0,
  -0.068,
  0.0,
  -0.026,
  0.0,
  -0.008,
  0.0,
  -0.002,
  0.0,
  0.0,
  0.0).map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))
  // TODO: might need to add an additional bit in order to make sure that the fixed point value wont be negative
  io.in.i.data.asFixedPoint(0.BP) // TODO: How does this conversion work? Does this produce an 8 bit FP with the integer component all above the point?
  val I_delay = Module (new GenericDelayChain(coeffs.length / 2, SInt((params.adcBits + 1).W)))
  I_scaled := io.in.i.data
  I_delay.io.in.valid :=  io.in.i.valid
  I_delay.io.in.bits := I_scaled - 15.S((params.adcBits + 1).W)
  var fir = Module( new GenericFIR(FixedPoint(12.W, 0.BP), FixedPoint(24.W, 11.BP), coeffs) )
  fir.io.in.valid := io.in.q.valid
  fir.io.in.bits := io.in.q.data

  Q_scaled := fir.io.out.bits.asSInt() >>> fir.io.out.bits.binaryPoint
  io.out.data.valid := I_delay.io.out.valid & fir.io.out.valid
  io.out.data.bits := I_delay.io.out.bits - Q_scaled
}
