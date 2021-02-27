package baseband

import chisel3._

object BasebandISA {
  // funct values
  val CONFIG_CMD = 0.U
  val SEND_CMD = 1.U
  val INTERRUPT_CMD = 7.U

  // rs2 value for CONFIG_CMD
  val CONFIG_CRC_PRESET = 0.U
  val CONFIG_ACCESS_ADDRESS = 1.U
  val CONFIG_CHANNEL_INDEX = 2.U
  val CONFIG_ADDITIONAL_FRAME_SPACE = 3.U
  val CONFIG_LOOPBACK_SELECT = 4.U
}