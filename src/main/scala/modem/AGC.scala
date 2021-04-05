package modem

import chisel3._
import chisel3.util._
import baseband.BLEBasebandModemParams
import chisel3.experimental.FixedPoint

class ACGMaxMinCell(dataBits: Int) extends Module {
  val io = IO(new Bundle {
    val data = new Bundle {
      val in = Flipped(Valid(UInt(dataBits.W)))
      val out = Valid(UInt(dataBits.W))
    }
    val max = new Bundle {
      val in = Input(UInt(dataBits.W))
      val out = Output(UInt(dataBits.W))
    }
    val min = new Bundle {
      val in = Input(UInt(dataBits.W))
      val out = Output(UInt(dataBits.W))
    }
  })

  val minReg = RegInit((scala.math.pow(2, dataBits) - 1).toInt.U(dataBits.W))
  val maxReg = RegInit(0.U(dataBits.W))
  val validReg = RegInit(false.B)

  when(io.data.in.fire()) {
    minReg := io.data.in.bits
    maxReg := io.data.in.bits
    validReg := io.data.in.valid
  }

  io.data.out.bits := minReg
  io.data.out.valid := validReg

  io.min.out := Mux(minReg < io.min.in, minReg, io.min.in)
  io.max.out := Mux(maxReg > io.max.in, maxReg, io.max.in)
}

class AGCControlIO(val params: BLEBasebandModemParams) extends Bundle {
  val sampleWindow = UInt(log2Ceil(params.agcMaxWindow).W)
  val idealPeakToPeak = UInt(params.adcBits.W)
  val gain = FixedPoint(8.W, 6.BP)
  val reset = Bool()
}

class AGCIO(params: BLEBasebandModemParams) extends Bundle {
  val adcIn = Flipped(Valid(UInt(params.adcBits.W)))
  val vgaLUTIndex = Output(UInt(5.W))
  val control = Input(new AGCControlIO(params))
}

/* Notes:
RMS > Peak to peek > max
RMS is fir with coeffienct = current value

Window: Make it adjustable through a memory mapped register
1 period of the IF frequency (2 MHz) on (200 MHz) gives some number of cycles is min
max is 5 periods (make it memory mapped adjustable)

RMS value gets error calculation relative to ideal RMS (memory mapped)
make gain memory mapped ( < 1 ) (fixed point)
goes to IF gain table (LUT) (ask Kerry how many settings he has)

20 bits take MSB of the Fixed point value

make two for I and Q
 */
class AGC(params: BLEBasebandModemParams) extends Module {
  val io = IO(new AGCIO(params))

  withReset(io.control.reset) {
    val maxMinBlocks = Seq.fill(params.cyclesPerSymbol * params.agcMaxWindow)(Module(new ACGMaxMinCell(params.adcBits)).io)

    maxMinBlocks.head.data.in <> io.adcIn
    maxMinBlocks.head.max.in := 0.U
    maxMinBlocks.head.min.in := (scala.math.pow(2, params.adcBits) - 1).toInt.U

    maxMinBlocks
      .zip(maxMinBlocks.tail)
      .foreach {
        case (left, right) =>
          right.data.in <> left.data.out
          right.max.in := left.max.out
          right.min.in := left.min.out
      }

    val peakToPeak = MuxLookup(io.control.sampleWindow, 1.U,
      Array.tabulate(params.agcMaxWindow)(i => {
        val index = ((i + 1) * params.cyclesPerSymbol - 1)
        (i+1).U -> (maxMinBlocks(index).max.out - maxMinBlocks(index).min.out)
      }))

    // To insure proper signed interpretation, we prepend each unsigned value with a 0 before subtraction
    val epsilon = Cat(0.U(1.W), peakToPeak) - Cat(0.U(1.W), io.control.idealPeakToPeak)

    // FixedPoint<17><<6>>
    val gainProduct = epsilon.asFixedPoint(0.BP) * io.control.gain

    // Normalized selection range around a center of 128 (assuming 8 bit ADC input) and gain of 1
    // This range is placed to to encapsulate the MSBs of the largest possible values (-128, 127)
    // The device cannot be more sensitive than this.
    // If the center is changed, the gain must be changed such that the (min, max) possible values are in the range (-128,127)
    // Once the device is in this max sensitivity state, sensitivity can be decreased by reducing the gain.
    io.vgaLUTIndex := gainProduct(gainProduct.getWidth - 4, gainProduct.getWidth - 8)
  }
}