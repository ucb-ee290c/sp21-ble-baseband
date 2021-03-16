package modem

import chisel3._
import chisel3.util._

import baseband.BLEBasebandModemParams

class AGCAverageCell(inputBits: Int, outputBits: Int) extends Module {
  val io = IO(new Bundle {
    val data = new Bundle {
      val in = Flipped(Valid(UInt(inputBits.W)))
      val out = Valid(UInt(inputBits.W))
    }
    val sum = new Bundle {
      val in = Input(UInt(outputBits.W))
      val out = Output(UInt(outputBits.W))
    }
  })

  val dataReg = RegInit(0.U(inputBits.W))
  val validReg = RegInit(false.B)

  when(io.data.in.fire()) {
    dataReg := io.data.in.bits
    validReg := io.data.in.valid
  }

  io.data.out.bits := dataReg
  io.data.out.valid := validReg

  io.sum.out := io.sum.in + dataReg
}

class AGCIO(adcBits: Int) extends Bundle {
  val adcIn = Flipped(Valid(UInt(adcBits.W)))
  val vgaAttenuation = Output(UInt(5.W))
  val reset = Input(Bool())
}

class AGC(params: BLEBasebandModemParams) extends Module {
  val io = IO(new AGCIO(params.adcBits))

  val avgRange = 6
  withReset(io.reset) {
    val cells = Seq.fill(avgRange)(Module(new AGCAverageCell(params.adcBits, log2Ceil(avgRange) + params.adcBits)).io)

    cells.head.data.in <> io.adcIn
    cells.head.sum.in := 0.U // Initial sum value

    cells
      .zip(cells.tail)
      .foreach {
        case (left, right) =>
          right.data.in <> left.data.out
          right.sum.in := left.sum.out
      }

    io.vgaAttenuation := cells.last.sum.out(4,0)
  }


}