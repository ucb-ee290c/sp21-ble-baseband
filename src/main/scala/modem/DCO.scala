package modem

import chisel3._
import chisel3.util._
import baseband.BLEBasebandModemParams
import chisel3.experimental.FixedPoint


class DCOControlIO extends Bundle {
  val gain = Input(FixedPoint(8.W, 6.BP))
  val reset = Input(Bool())
}

class DCOIO(params: BLEBasebandModemParams) extends Bundle {
  val adcIn = Flipped(Valid(UInt(params.adcBits.W)))
  val dcoLUTIndex = Output(UInt(5.W))
  val control = Input(new DCOControlIO)
}


/* DC offset loop (valid only)
Integrate over input (forever), apply gain, LUT, back to analog
ADC input minus half goes into integrator goes into truncation (MSB) into LUT
 */
class DCO(params: BLEBasebandModemParams) extends Module {
  val io = IO(new DCOIO(params))

  withReset(io.control.reset) {
    val integration = RegInit(0.U((2 * params.adcBits).W))

    // To insure proper signed interpretation, we prepend each unsigned value with a 0 before subtraction
    val epsilon = Cat(0.U(1.W), io.adcIn.bits) - scala.math.pow(2, params.adcBits -1 ).toInt.U((params.adcBits + 1).W)

    when (io.adcIn.fire()) {
      integration := integration + epsilon
    }

    // FixedPoint<17><<6>>
    val gainProduct = integration.asFixedPoint(0.BP) * io.control.gain

    io.dcoLUTIndex := gainProduct.asUInt().apply(gainProduct.getWidth - 1, gainProduct.getWidth - 5)
  }
}
