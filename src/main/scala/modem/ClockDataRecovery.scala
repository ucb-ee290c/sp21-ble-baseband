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
  io.out.carry := Counter(!io.in.phaseError, 5)._2 // Up Counter
  io.out.borrow := Counter(io.in.phaseError, 5)._2 // Down Counter
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

  val incDetected = RegInit(0.B)
  val decDetected = RegInit(0.B)

  val toggleFF = Wire(Bool())
  toggleFF := RegEnable(!toggleFF, 0.B, !(incDetected & !toggleFF | decDetected & toggleFF))

  when (risingedge(io.in.inc)) {
    incDetected := 1.B
  }.elsewhen(incDetected & !toggleFF) {
    incDetected := 0.B
  }

  when (risingedge(io.in.dec)) {
    decDetected := 1.B
  }.elsewhen(decDetected & toggleFF) {
    decDetected := 0.B
  }

  io.out.clk := RegEnable(!io.out.clk, risingedge(Counter(!toggleFF & !clock.asBool(), 5)._2))

}

class CDR extends Module {
  val io =  IO(new Bundle() {
    val d = Input(Bool())
    val clk = Output(Bool())
  })

  val kCounter = Module(new KCounter).io
  val dco = Module(new CDRDCO).io

  kCounter.in.phaseError := io.d ^ dco.out.clk
  dco.in.inc := kCounter.out.carry
  dco.in.dec := kCounter.out.borrow

  io.clk := ShiftRegister(dco.out.clk, 15)

}
