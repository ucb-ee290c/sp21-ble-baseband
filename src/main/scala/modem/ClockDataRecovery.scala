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
  io.out.carry := Counter(up, 10)._2 // Up Counter
  io.out.borrow := Counter(down, 10)._2 // Down Counter
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

  val willInc = RegInit(0.B)
  val willDec = RegInit(0.B)

  val toggleFF = Wire(Bool())
  val shouldToggle = !(willInc & !toggleFF | willDec & toggleFF)
  toggleFF := RegEnable(!toggleFF, 0.B, shouldToggle)

  when (risingedge(io.in.inc)) {
    willInc := 1.B
  }.elsewhen(!toggleFF) {
    willInc := 0.B
  }

  when (risingedge(io.in.dec)) {
    willDec := 1.B
  }.elsewhen(toggleFF) {
    willDec := 0.B
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

  io.clk := RegEnable(!io.clk, 1.B, risingedge(Counter(dco.out.clk, 10)._2))

}
