/* Based on (with permission) the Fall 2018 290C BLE Baseband
   https://github.com/ucberkeley-ee290c/fa18-ble/
 */
package baseband // belongs to folder

import chisel3._
import chisel3.util._


class Whitening extends Module {
  val io = IO(new Bundle {
    val operand  = Flipped(Valid(UInt(1.W)))
    val result   = Valid(UInt(1.W))
    val seed     = Input(UInt(7.W))
    val init     = Input(Bool()) // TODO: Why can't this seed, init combo be replaced with a valid interface on seed?
  })

  val whitening_lfsr = RegInit(0.U(7.W)) // 1+x^4+x^7;
  val inv = whitening_lfsr(0)

  when (io.init) {
    whitening_lfsr := Reverse(io.seed)
  }.elsewhen (io.operand.fire()) {
    whitening_lfsr := Cat(inv, whitening_lfsr(6), whitening_lfsr(5), whitening_lfsr(4), whitening_lfsr(3)^inv, whitening_lfsr(2), whitening_lfsr(1))
  }

  io.result.valid := io.operand.fire()
  io.result.bits := Mux(io.operand.valid, inv ^ io.operand.bits, 0.U) // TODO: Verify correctness of simplifications

}