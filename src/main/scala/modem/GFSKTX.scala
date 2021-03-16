package modem

import chisel3._
import chisel3.util._

class AnalogTXIO extends Bundle {
  val freqOffset = Output(UInt(8.W)) // TODO: Establish this value
  val pllReady = Input(Bool())
}

class GFSKTX extends Module {
  val io = IO(new Bundle {
    val analog = new Bundle {
      val tx = new AnalogTXIO
    }
    val digital = new Bundle {
      val in = Flipped(Decoupled(UInt(1.W)))
    }
  })

  io.digital.in.ready := false.B
  io.analog.tx.freqOffset := 0.U
}