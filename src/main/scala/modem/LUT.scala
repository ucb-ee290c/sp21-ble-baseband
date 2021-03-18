package modem

import chisel3._
import chisel3.util._

object LUT {
  val LOFSK = 0.U
  val LOCT = 1.U
}

class GFSKModemLUTs extends Bundle {
  val LOFSK = Vec(64, UInt(8.W))
  val LOCT = Vec(64, UInt(8.W))
}

class GFSKModemLUTCommand extends Bundle {
  /* LUT Command: used to write to a LUT within the modem.
    [31:10 - Value to be written to LUT | 9:4 - address in LUT | 3:0 - LUT index]
   */
  val lut = UInt(4.W)
  val address = UInt(6.W)
  val value = UInt(22.W)
}

class GFSKModemLUTIO extends Bundle {
  val setLUTCmd = Flipped(Decoupled(new GFSKModemLUTCommand))
}