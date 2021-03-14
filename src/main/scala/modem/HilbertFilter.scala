package modem


import baseband.{BLEBasebandModemParams, PDAControlInputBundle}
import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import firrtl.ir.Width
import freechips.rocketchip.config.Parameters
import chipyard.example.GenericFIR


class FPFIRCellBundle() extends Bundle {
  val data = FixedPoint(6.W, 0.BP)
  val carry = FixedPoint(18.W, 11.BP)
}

class FPFIRCellIO() extends Bundle {
  val coeff = Input(FixedPoint(12.W, 11.BP))
  val in = Flipped(Decoupled(new FPFIRCellBundle() ))
  val out = Decoupled(new FPFIRCellBundle() )
}

class FPFIRIO() extends Bundle {
  val in = Flipped(Decoupled( FixedPoint(12.W, 11.BP) ))
  val out = Decoupled(FixedPoint(18.W, 11.BP))
}

// A generic FIR filter
// DOC include start: GenericFIR chisel
class FPFIR(coeffs: Seq[FixedPoint(12.W, 11.BP)]) extends Module {
  val io = IO(new FPFIRIO())

  // Construct a vector of genericFIRDirectCells
  val directCells = Seq.fill(coeffs.length){ Module(new FPFIRDirectCell).io }

  // Construct the direct FIR chain
  for ((cell, coeff) <- directCells.zip(coeffs)) {
    cell.coeff := coeff
  }

  // Connect input to first cell
  directCells.head.in.bits.data := io.in.bits
  directCells.head.in.bits.carry := 0.F(0.BP)
  directCells.head.in.valid := io.in.valid
  io.in.ready := directCells.head.in.ready

  // Connect adjacent cells
  // Note that .tail() returns a collection that consists of all
  // elements in the inital collection minus the first one.
  // This means that we zip together directCells[0, n] and
  // directCells[1, n]. However, since zip ignores unmatched elements,
  // the resulting zip is (directCells[0], directCells[1]) ...
  // (directCells[n-1], directCells[n])
  for ((current, next) <- directCells.zip(directCells.tail)) {
    next.in.bits := current.out.bits
    next.in.valid := current.out.valid
    current.out.ready := next.in.ready
  }

  // Connect output to last cell
  io.out.bits := directCells.last.out.bits.carry
  directCells.last.out.ready := io.out.ready
  io.out.valid := directCells.last.out.valid

}
// DOC include end: GenericFIR chisel

// A generic FIR direct cell used to construct a larger direct FIR chain
//
//   in ----- [z^-1]-- out
//	        |
//   coeff ----[*]
//	        |
//   carryIn --[+]-- carryOut
//
// DOC include start: GenericFIRDirectCell chisel
class FPFIRDirectCell() extends Module {
  val io = IO(new FPFIRCellIO())

  // Registers to delay the input and the valid to propagate with calculations
  val hasNewData = RegInit(0.U)
  val inputReg = Reg(FixedPoint(12.W, 0.BP))

  // Passthrough ready
  io.in.ready := io.out.ready

  // When a new transaction is ready on the input, we will have new data to output
  // next cycle. Take this data in
  when (io.in.fire()) {
    hasNewData := 1.U
    inputReg := io.in.bits.data
  }

  // We should output data when our cell has new data to output and is ready to
  // recieve new data. This insures that every cell in the chain passes its data
  // on at the same time
  io.out.valid := hasNewData & io.in.fire()
  io.out.bits.data := inputReg

  // Compute carry
  // This uses the ring implementation for + and *, i.e.
  // (a * b) maps to (Ring[T].prod(a, b)) for whicever T you use
  io.out.bits.carry := inputReg * io.coeff + io.in.bits.carry
}

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
  var fir = Module( new FPFIR(coeffs) )

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
