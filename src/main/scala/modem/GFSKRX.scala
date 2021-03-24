package modem

import chisel3._
import chisel3.util._

import baseband.BLEBasebandModemParams


class GFSKRX(params: BLEBasebandModemParams) extends Module {
  val io = IO(new Bundle {
    val analog = new Bundle {
      val i = Flipped(Decoupled(UInt(params.adcBits.W)))
      val q = Flipped(Decoupled(UInt(params.adcBits.W)))
    }
    val digital = new Bundle {
      val out = Decoupled(UInt(1.W))
    }
  })

  io.digital.out.valid := false.B
  io.digital.out.bits := 0.U
  io.analog.i.ready := false.B
  io.analog.q.ready := false.B




}