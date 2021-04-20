package modem

import chisel3._
import chisel3.util._
import chisel3.Module

class CDRDCO extends Module {
  val io = IO(new Bundle {
    val in = Input(new Bundle {
      val inc = Bool()
      val dec = Bool()
    })
    val out = Output(new Bundle() {
      val clk = Bool()
    })
  })
  val willInc = RegInit(0.B)
  val willDec = RegInit(0.B)

  val toggleFF = Wire(Bool())
  val shouldToggle = !(willInc & !toggleFF | willDec & toggleFF)
  toggleFF := RegEnable(!toggleFF, 0.B, shouldToggle)

  when (Utility.risingedge(io.in.inc)) {
    willInc := 1.B
  }.elsewhen(Counter(0 until 2, !toggleFF, Utility.risingedge(io.in.inc))._2) {
    willInc := 0.B
  }

  when (Utility.risingedge(io.in.dec)) {
    willDec := 1.B
  }.elsewhen(Counter(0 until 2, toggleFF, Utility.risingedge(io.in.dec))._2) {
    willDec := 0.B
  }

  io.out.clk := !toggleFF & !clock.asBool()
    //RegEnable(!io.out.clk, risingedge(Counter(!toggleFF & !clock.asBool(), 5)._2))

}

class FPSCDR extends Module {
  val io = IO(new Bundle {
    val d = Input(Bool())
    val clk = Output(Bool())
  })
  val s_idle :: s_tracking :: s_shifting :: Nil = Enum(3)
  val state = RegInit(s_idle)

  val dco = Module(new CDRDCO).io

  io.clk := RegEnable(!io.clk, 1.B, Utility.risingedge(Counter(dco.out.clk, 5)._2))

  when (Utility.risingedge(io.clk)) {
    when (state === s_idle) {
      state := s_tracking
    } .elsewhen (state === s_tracking) {
      state := s_shifting
    } .otherwise {
      state := s_tracking
    }
  }

  val accumulator = Module(new Accumulator)

  val beginCount  = state

  val earlyDone = Wire(Bool())
  val earlyCounter = Wire(UInt(8.W))
  withReset(Utility.risingedge(state === s_tracking)) {
    earlyCounter := RegEnable(earlyCounter + 1.U, 0.U, !(earlyCounter === 17.U))
  }
  earlyDone := (earlyCounter === 17.U)
  val promptDone = Wire(Bool())
  val promptCounter = Wire(UInt(8.W))
  withReset(Utility.risingedge(state === s_tracking)) {
    promptCounter := RegEnable(promptCounter + 1.U, 0.U, !(promptCounter === 19.U))
  }
  promptDone := (promptCounter === 19.U)
  val lateDone = Wire(Bool())
  val lateCounter = Wire(UInt(8.W))
  withReset(Utility.risingedge(state === s_tracking)) {
    lateCounter := RegEnable(lateCounter + 1.U, 0.U, !(lateCounter === 21.U))
  }
  lateDone := (lateCounter === 21.U)

  accumulator.io.d := io.d
  val early = RegEnable(accumulator.io.sum, 0.S, Utility.risingedge(earlyDone))
  val prompt = RegEnable(accumulator.io.sum, 0.S, Utility.risingedge(promptDone))
  val late = RegEnable(accumulator.io.sum, 0.S, Utility.risingedge(lateDone))

  val ready = ShiftRegister(earlyDone && promptDone && lateDone, 1)

  dco.in.inc := ready && (early.abs() > prompt.abs() )
  dco.in.dec := ready && (late.abs() > prompt.abs() ) && (late.abs() > early.abs())
}

class Accumulator extends Module {
  val io = IO(new Bundle {
    val d = Input(UInt(1.W))
    val sum = Output(SInt(8.W))
  })
  val cells = Seq.tabulate(20){i => Module(new AccumulatorCell).io}
  cells.head.d := RegNext(io.d)
  cells.head.previousSum := 0.S

  for ((current, next) <- cells.zip(cells.tail)) {
    next.d := current.output
    next.previousSum := current.sum
  }
  io.sum := cells.last.sum
}
class AccumulatorCell extends Module {
  val io = IO(new Bundle {
    val d = Input(UInt(1.W))
    val previousSum = Input(SInt(8.W))
    val sum = Output(SInt(8.W))
    val output = Output(UInt(1.W))
  })
  io.output := RegNext(io.d)
  io.sum := io.previousSum + Mux(io.d.asBool(), 1.S, (-1).S)
}