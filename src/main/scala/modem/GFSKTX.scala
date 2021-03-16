package modem

import chisel3._
import chisel3.util._

class GFSKTX extends Module {
  val io = IO(new Bundle {
    val analog = new Bundle {
      val gfskIndex = Output(UInt(6.W))
    }
    val digital = new Bundle {
      val in = Flipped(Decoupled(UInt(1.W)))
    }
  })

  io.digital.in.ready := false.B
  io.analog.gfskIndex := 0.U
}