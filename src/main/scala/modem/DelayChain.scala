package modem

import chisel3._
import chisel3.util._

class GenericDelayChainIO[T<:Data](t: T) extends Bundle {
  val in = Flipped(Decoupled(t))
  val out = Decoupled(t)
}

class GenericDelayChain[T<:Data](cycles: Int, t: T) extends Module {
  val io = IO(new GenericDelayChainIO(t))

  val delayRegisters = Seq.fill(cycles) {Module(new GenericDelayChainCell(t)).io}

  delayRegisters.head.in <> io.in

  for ((current, next) <- delayRegisters.zip(delayRegisters.tail)) {
    next.in <> current.out
  }

  io.out <> delayRegisters.last.out
}

class GenericDelayChainCell[T<:Data](t: T) extends Module {
  val io = IO(new GenericDelayChainIO(t))

  val valuePresent = RegInit(0.U)
  val inputReg = Reg(t)

  io.in.ready := io.out.ready

  when (io.in.fire()) {
    valuePresent := 1.U
    inputReg := io.in.bits
  }
  io.out.valid := valuePresent & io.in.fire()
  io.out.bits := inputReg

}
