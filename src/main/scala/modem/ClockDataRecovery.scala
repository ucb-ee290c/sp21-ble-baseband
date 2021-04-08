package modem

import chisel3._
import chisel3.util._
import chisel3.Module

class KCounter extends Module {
  val io = IO(new Bundle {
    val in = Input(new Bundle {
      val phaseError = Bool()
    })
    val out = Output(new Bundle() {
      val carry = Bool()
      val borrow = Bool()
    })
  })
  val up = !io.in.phaseError
  val down = io.in.phaseError
  io.out.carry := Counter(up, 5)._2 // Up Counter
  io.out.borrow := Counter(down, 5)._2 // Down Counter
}

class CDRDCO extends Module {
  def risingedge(x: Bool) = x && !RegNext(x)
  val io = IO(new Bundle {
    val in = Input(new Bundle {
      val inc = Bool()
      val dec = Bool()
    })
    val out = Output(new Bundle() {
      val clk = Bool()
    })
  })

  val shouldInc = RegInit(0.B)
  val shouldDec = RegInit(0.B)
  val willWillInc = RegInit(0.B)
  val willWillDec = RegInit(0.B)
  val willInc = RegInit(0.B)
  val willDec = RegInit(0.B)

  val toggleFF = Wire(Bool())
  val shouldToggle = !(willInc & !toggleFF | willDec & toggleFF)
  toggleFF := RegEnable(!toggleFF, 0.B, shouldToggle)

  when (risingedge(io.in.inc)) {
    willWillInc := 1.B
    willInc := 1.B
    shouldInc := 1.B
  }.elsewhen(!toggleFF) {
    when (shouldInc) {
      shouldInc := 0.B
    }.elsewhen(willWillInc) {
      willWillInc := 0.B
    }.otherwise {
      willInc := 0.B
    }
  }

  when (risingedge(io.in.dec)) {
    willDec := 1.B
    shouldDec := 1.B
  }.elsewhen(toggleFF) {
    when (shouldDec) {
      shouldDec := 0.B
    }.elsewhen(willWillDec) {
      willWillDec := 0.B
    }.otherwise {
      willDec := 0.B
    }
  }

  io.out.clk := !toggleFF & !clock.asBool()
    //RegEnable(!io.out.clk, risingedge(Counter(!toggleFF & !clock.asBool(), 5)._2))

}

class CDR extends Module {
  def risingedge(x: Bool) = x && !RegNext(x)
  val io =  IO(new Bundle() {
    val d = Input(Bool())
    val clk = Output(Bool())
  })

  val kCounter = Module(new KCounter).io
  val dco = Module(new CDRDCO).io

  kCounter.in.phaseError := io.d ^ io.clk
  dco.in.inc := kCounter.out.carry
  dco.in.dec := kCounter.out.borrow

  io.clk := RegEnable(!io.clk, 1.B, risingedge(Counter(dco.out.clk, 5)._2))

}


class FPSCDR extends Module {
  def risingedge(x: Bool) = x && !RegNext(x)
  val io = IO(new Bundle {
    val d = Input(Bool())
    val clk = Output(Bool())
  })
  val dco = Module(new CDRDCO).io
  val s_idle :: s_tracking :: s_shifting :: Nil = Enum(3)
  val state = RegInit(s_idle)

  when (risingedge(io.clk)) {
    when (state === s_idle) {
      state := s_tracking
    } .elsewhen (state === s_tracking) {
      state := s_shifting
    } .otherwise {
      state := s_tracking
    }
  }

  val accumulator = Module(new Accumulator)

  val earlyWrap = Wire(Bool())
  val earlyCounter = Wire(UInt(8.W))
  withReset(risingedge(state === s_tracking)) {
    earlyCounter := RegEnable(earlyCounter + 1.U, 0.U, !(earlyCounter === 17.U))
  }
  earlyWrap := (earlyCounter === 17.U)
  val promptWrap = Wire(Bool())
  val promptCounter = Wire(UInt(8.W))
  withReset(risingedge(state === s_tracking)) {
    promptCounter := RegEnable(promptCounter + 1.U, 0.U, !(promptCounter === 19.U))
  }
  promptWrap := (promptCounter === 19.U)
  val lateWrap = Wire(Bool())
  val lateCounter = Wire(UInt(8.W))
  withReset(risingedge(state === s_tracking)) {
    lateCounter := RegEnable(lateCounter + 1.U, 0.U, !(lateCounter === 21.U))
  }
  lateWrap := (lateCounter === 21.U)

  accumulator.io.d := io.d
  val early = RegEnable(accumulator.io.sum, 0.S, risingedge(earlyWrap))
  val prompt = RegEnable(accumulator.io.sum, 0.S, risingedge(promptWrap))
  val late = RegEnable(accumulator.io.sum, 0.S, risingedge(lateWrap))

  dco.in.inc := ShiftRegister(promptWrap && lateWrap && earlyWrap, 1) && (early.abs() > prompt.abs() )
  dco.in.dec := ShiftRegister(promptWrap && lateWrap && earlyWrap, 1) && (late.abs() > prompt.abs() ) && (late.abs() > early.abs())
  io.clk := RegEnable(!io.clk, 1.B, risingedge(Counter(dco.out.clk, 5)._2))
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