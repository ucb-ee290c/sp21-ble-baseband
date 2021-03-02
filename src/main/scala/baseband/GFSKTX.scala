package baseband

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters

class AnalogTXIO extends Bundle {
  val freqCenter = Output(UInt(6.W))
  val freqOffset = Output(UInt(6.W)) // TODO: Establish this value
  val pllReady = Input(Bool())
}

class GFSKTX()(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val analog = new Bundle {
      val tx = new AnalogTXIO
    }
    val digital = new Bundle {
      val in = Decoupled(UInt(1.W))
    }
  })
}