package modem

import chisel3._
import chisel3.util._

import baseband.BLEBasebandModemParams

class AnalogRXIO(params: BLEBasebandModemParams) extends Bundle {
  val i = new Bundle {
    val data = Input(UInt(params.adcBits.W))
    val valid = Input(Bool())
  }
  val q = new Bundle {
    val data = Input(UInt(params.adcBits.W))
    val valid = Input(Bool())
  }
}

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
  io.analog.i.ready := false.B
  io.analog.q.ready := false.B
}