//// See LICENSE for license details.
//
package modem

import chisel3._
import chisel3.experimental.FixedPoint
import chisel3.util._
import dsptools.numbers._
import freechips.rocketchip.config.{Parameters, Field, Config}

// FIR params
case class GenericFIRParams(
                             writeAddress: BigInt = 0x2000,
                             readAddress: BigInt = 0x2100,
                             depth: Int
                           )

case object GenericFIRKey extends Field[Option[GenericFIRParams]](None)

class GenericFIRCellBundle[T<:Data:Ring](genIn:T, genOut:T) extends Bundle {
  val data: T = genIn.cloneType
  val carry: T = genOut.cloneType

  override def cloneType: this.type = GenericFIRCellBundle(genIn, genOut).asInstanceOf[this.type]
}
object GenericFIRCellBundle {
  def apply[T<:Data:Ring](genIn:T, genOut:T): GenericFIRCellBundle[T] = new GenericFIRCellBundle(genIn, genOut)
}

class GenericFIRCellIO[T<:Data:Ring](genIn:T, genOut:T, c:T) extends Bundle {
  val coeff = Input(c.cloneType)
  val in = Flipped(Decoupled(GenericFIRCellBundle(genIn, genOut)))
  val out = Decoupled(GenericFIRCellBundle(genIn, genOut))
}
object GenericFIRCellIO {
  def apply[T<:Data:Ring](genIn:T, genOut:T, c:T): GenericFIRCellIO[T] = new GenericFIRCellIO(genIn, genOut, c)
}

class GenericFIRBundle[T<:Data:Ring](proto: T) extends Bundle {
  val data: T = proto.cloneType

  override def cloneType: this.type = GenericFIRBundle(proto).asInstanceOf[this.type]
}
object GenericFIRBundle {
  def apply[T<:Data:Ring](proto: T): GenericFIRBundle[T] = new GenericFIRBundle(proto)
}

class GenericFIRIO[T<:Data:Ring](genIn:T, genOut:T) extends Bundle {
  val in = Flipped(Decoupled(GenericFIRBundle(genIn)))
  val out = Decoupled(GenericFIRBundle(genOut))
}
object GenericFIRIO {
  def apply[T<:Data:Ring](genIn:T, genOut:T): GenericFIRIO[T] = new GenericFIRIO(genIn, genOut)
}

// A generic FIR filter
// DOC include start: GenericFIR chisel
class GenericFIR[T<:Data:Ring](genIn:T, genOut:T, coeffs: Seq[T]) extends Module {
  val io = IO(GenericFIRIO(genIn, genOut))

  // Construct a vector of genericFIRDirectCells
  val directCells = Seq.tabulate(coeffs.length){ i: Int => Module(new GenericFIRDirectCell(genIn, genOut, coeffs(i))).io }

  // Construct the direct FIR chain
  for ((cell, coeff) <- directCells.zip(coeffs)) {
    cell.coeff := coeff
  }

  // Connect input to first cell
  directCells.head.in.bits.data := io.in.bits.data
  directCells.head.in.bits.carry := Ring[T].zero
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
  io.out.bits.data := directCells.last.out.bits.carry
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
class GenericFIRDirectCell[T<:Data:Ring](genIn: T, genOut: T, c: T) extends Module {
  val io = IO(GenericFIRCellIO(genIn, genOut, c))

  // Registers to delay the input and the valid to propagate with calculations
  val hasNewData = RegInit(0.U)
  val inputReg = Reg(genIn.cloneType)

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
