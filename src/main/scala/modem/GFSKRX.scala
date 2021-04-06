package modem

import chisel3._
import chisel3.util._
import chisel3.experimental.{DataMirror, FixedPoint}
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
    val control = new Bundle {
      val in = new Bundle {
        val imageRejectionOp = Input(Bool())
      }
      val out = new Bundle {
        val preambleDetected = Output(Bool())
      }
    }
  })

  val imageRejection = Module (new HilbertFilter(params))
  imageRejection.io.in <> io.analog

  val cdr = Module(new CDRDecision(params))
  cdr.io.signal.bits := imageRejection.io.out.data.bits(6,1).asSInt()
  cdr.io.signal.valid := imageRejection.io.out.data.valid
  imageRejection.io.out.data.ready := cdr.io.signal.ready

  io.digital.out <> cdr.io.out
  io.control.out.preambleDetected := false.B //TODO
}