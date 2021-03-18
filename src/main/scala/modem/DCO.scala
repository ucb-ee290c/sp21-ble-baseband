package modem

import chisel3._
import chisel3.util._
import baseband.BLEBasebandModemParams
import chisel3.experimental.FixedPoint


class DCOIO(params: BLEBasebandModemParams) extends Bundle {
  val adcIn = Flipped(Valid(UInt(params.adcBits.W)))
  val dcoLUTIndex = Output(UInt(5.W))
  val integration = Output(UInt((2*params.adcBits).W))
  val epsilon = Output(UInt((params.adcBits + 1).W))
  val control = new Bundle {
    val gain = Input(FixedPoint(8.W, 6.BP))
  }
  val reset = Input(Bool())
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
class DCO(params: BLEBasebandModemParams) extends Module {
  val io = IO(new DCOIO(params))

  withReset(io.reset) {
    val integration = RegInit(0.U((2*params.adcBits).W))

    val epsilon = Cat(0.U(1.W), io.adcIn.bits) - 128.U((params.adcBits + 1).W)
    when (io.adcIn.fire()) {
      integration := integration + epsilon
    }
    // To insure proper signed interpretation, we prepend each unsigned value with a 0 before subtraction

    // FixedPoint<17><<6>>
    val gainProduct = integration.asFixedPoint(0.BP) * io.control.gain

    io.integration := integration
    io.epsilon := epsilon
    io.dcoLUTIndex := gainProduct.asUInt().apply(gainProduct.getWidth - 1, gainProduct.getWidth - 5)
  }
}

/* DC offset loop (valid only)
Integrate over input (forever), apply gain, LUT, back to analog
ADC input minus half goes into integrator goes into truncation (MSB) into LUT
 */