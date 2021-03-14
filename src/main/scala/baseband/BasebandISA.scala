package baseband

import chisel3._
import chisel3.util._

object BasebandISA {
  // primaryInst values and corresponding instructions

  /* Configure command:
      Configure baseband constants. If secondaryInst = CONFIG_LO_LUT, the data field holds the
      LO LUT address to be set, else the data field is a Don't Care.
      [ Data = <LO LUT address / X> | secondaryInst = <target constant> | primaryInst = 0 ]
      [ additionalData = <value> ]
   */
  val CONFIG_CMD = 0.U

  // secondaryInst values for CONFIG_CMD
  val CONFIG_CRC_SEED = 0.U
  val CONFIG_ACCESS_ADDRESS = 1.U
  val CONFIG_CHANNEL_INDEX = 2.U
  val CONFIG_ADDITIONAL_FRAME_SPACE = 3.U
  val CONFIG_LO_LUT = 4.U

  /* Send command:
      Transmit a specified number of PDU header and data bytes. Bytes are gathered by loading them from the
      specified address
      [ Data = <total bytes> | secondaryInst = X | primaryInst = 1 ]
      [ additionalData = <load address> ]
   */
  val SEND_CMD = 1.U

  /* Receive command:
      Place the device into receive mode. If a message is picked up, it will be stored starting at
      the specified storage address.
      [ Data = X | secondaryInst = X | primaryInst = 2 ]
      [ additionalData = <storage address> ]
 */
  val RECEIVE_CMD = 2.U

  /* Receive exit command:
      Exit the device from receive mode.
      [ Data = X | secondaryInst = X | primaryInst = 3 ]
      [ additionalData = X ]
 */
  val RECEIVE_EXIT_CMD = 3.U

  /* Debug command:
      Turns on both the RX and TX paths according to the loopback mask and passes the specified number of PDU
      header and data bytes in a loop. For simplicity the return data is stored at <load address + total bytes>,
      rounded to the nearest byte aligned address.
      [ Data = <total bytes> | secondaryInst = <loopback mask> | primaryInst = 15 ]
      [ additionalData = <load address> ]
   */
  val DEBUG_CMD = 15.U

  // Interrupt reason codes
  val INTERRUPT_REASON_INVALID_TX_LENGTH = 0

  def INTERRUPT(reason: Int, message: Int = 0): UInt = {
    Cat(message.U(26.W), reason.U(6.W))
  }
}