package modem


import baseband.PDAControlInputBundle
import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import firrtl.ir.Width
import freechips.rocketchip.config.Parameters
//import chipyard.example.GenericFIR

class HilbertFilterControlInput extends Bundle {
  val combineOperation = UInt(1.W)
}

class QuadratureSignalsInput(bitWidth: Int) extends Bundle {
  val I = Input(UInt(bitWidth.W))
  val Q = Input(UInt(bitWidth.W))
}

class HilbertFilterInput(bitWidth: Int) extends Bundle {
  val control = Input(new HilbertFilterControlInput)
  val signals = Flipped(Decoupled(new QuadratureSignalsInput(bitWidth)))
}

class HilbertFilterOutput(bitWidth: Int) extends Bundle {
  val data = Decoupled(UInt(bitWidth.W))
}

class HilbertFilterIO(bitWidth: Int) extends Bundle {
  val in = new HilbertFilterInput(bitWidth)
  val out = new HilbertFilterOutput(bitWidth)
}



class HilbertFilter() extends Module {
  var io = IO(new HilbertFilterIO(12))

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
  io.in.signals.bits.I.asFixedPoint(0.BP) // TODO: How does this conversion work? Does this produce an 8 bit FP with the integer component all above the point?
  io.in.signals.bits.Q.asFixedPoint(0.BP)
  val I_delay = Module (new GenericDelayChain(coeffs.length / 2, io.in.signals.bits.I))

  //var fir = Module( new GenericFIR(FixedPoint(12.W, 0.BP), FixedPoint(24.W, 11.BP), coeffs) )
  I_delay.io.in.valid :=  io.in.signals.valid
  I_delay.io.in.bits := io.in.signals.bits.I

 // fir.io.in.valid := io.in.signals.valid
 // fir.io.in.bits := io.in.signals.bits.I

  io.out.data <> I_delay.io.out

}
