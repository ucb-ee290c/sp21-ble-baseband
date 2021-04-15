package modem

import chisel3._
import chisel3.util._
import chisel3.experimental.{DataMirror, FixedPoint}
import baseband.BLEBasebandModemParams

class GFSKRXControlInputBundle extends Bundle {
  val enable = Bool()
  val imageRejectionOp = Bool()
  val accessAddressLSB = UInt(1.W)
  val preambleDetectionThreshold = UInt(log2Ceil(20 * 8 + 1).W) // TODO: THIS SHOULD BE MMIO
}

class GFSKRXControlOutputBundle extends Bundle {
  val preambleDetected = Bool()
}

class GFSKRXControlIO extends Bundle {
  val in = Input(new GFSKRXControlInputBundle)
  val out = Output(new GFSKRXControlOutputBundle)
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
    val control = new GFSKRXControlIO
  })

  val imageRejection = Module (new HilbertFilter(params))
  imageRejection.io.in.i.valid := io.analog.i.valid
  imageRejection.io.in.q.valid := io.analog.q.valid
  imageRejection.io.in.i.bits := io.analog.i.bits
  imageRejection.io.in.q.bits := io.analog.q.bits
  imageRejection.io.control.operation := io.control.in.imageRejectionOp
  io.analog.i.ready := imageRejection.io.in.i.ready
  io.analog.q.ready := imageRejection.io.in.q.ready

  val demod = Module(new GFSKDemodulation(params))
  demod.io.signal.bits := imageRejection.io.out.bits(6,1).asSInt()
  demod.io.signal.valid := imageRejection.io.out.valid
  imageRejection.io.out.ready := demod.io.signal.ready

  def detectEdge(x: Bool) = x =/= RegNext(x)
  def risingedge(x: Bool) = x && !RegNext(x)
  def fallingedge(x: Bool) = !x && RegNext(x)

  val guess = Wire(Bool())
  val cdr = Module(new FPSCDR)
  val beginSampling = Wire(Bool())
  guess := demod.io.guess.bits
  demod.io.guess.ready := io.control.in.enable

  val accumulator = Wire(SInt(8.W)) // TODO: THIS WIDTH IS VERY IMPORTANT, THIS VALUE CANNOT OVERFLOW
  accumulator := RegNext(Mux(beginSampling, 0.S, accumulator + Mux(guess, 1.S, (-1).S).asSInt()), 0.S(8.W))
  cdr.io.d :=  guess
  beginSampling := risingedge(cdr.io.clk)
  io.digital.out.valid := beginSampling
  io.digital.out.bits := Mux(accumulator > 0.S, 1.U, 0.U)

  val preambleDetector = Module(new PreambleDetector())
  val preambleDetected = RegInit(0.B)
  val preambleValid = RegInit(0.B)

  preambleDetector.io.control.firstBit := io.control.in.accessAddressLSB
  preambleDetector.io.in := guess
  preambleDetector.io.control.threshold := io.control.in.preambleDetectionThreshold
  preambleDetector.io.control.reset := beginSampling

  when (preambleDetector.io.detected) {
    preambleDetected := 1.B
  }
  when (fallingedge(cdr.io.clk)) {
    preambleValid := preambleDetected
  }
  io.control.out.preambleDetected := preambleValid
}