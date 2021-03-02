package baseband

import chisel3._
import chisel3.util._

object BasebandISA {
  // funct values
  val CONFIG_CMD = 0.U
  val SEND_CMD = 1.U
  val INTERRUPT_CMD = 6.U

  // rs2 value for CONFIG_CMD
  val CONFIG_CRC_SEED = 0.U
  val CONFIG_WHITENING_SEED = 1.U
  val CONFIG_ACCESS_ADDRESS = 2.U
  val CONFIG_CHANNEL_INDEX = 3.U
  val CONFIG_ADDITIONAL_FRAME_SPACE = 4.U
  val CONFIG_LOOPBACK_SELECT = 5.U

  // Interrupt reason codes
  val INTERRUPT_REASON_INVALID_TX_LENGTH = 0

  def INTERRUPT(reason: Int, message: Int = 0): UInt = {
    Cat(message.U(26.W), reason.U(6.W))
  }
}