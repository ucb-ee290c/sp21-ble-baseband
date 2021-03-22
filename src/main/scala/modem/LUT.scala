package modem

import chisel3._
import chisel3.util._

object GFSKModemLUTCodes {
  val LOFSK = 0.U
  val LOCT = 1.U
  val AGCI = 2.U
  val AGCQ = 3.U
}

class GFSKModemLUTs extends Bundle {
  val LOFSK = Vec(64, UInt(8.W))
  val LOCT = Vec(64, UInt(8.W))
  val AGCI = Vec(64, UInt(5.W))
  val AGCQ = Vec(64, UInt(5.W))
}

class GFSKModemLUTCommand extends Bundle {
  /* LUT Command: used to write to a LUT within the modem.
    [31:10 - Value to be written to LUT | 9:4 - address in LUT | 3:0 - LUT index]
   */
  val lut = UInt(4.W)
  val address = UInt(6.W)
  val value = UInt(22.W)
}