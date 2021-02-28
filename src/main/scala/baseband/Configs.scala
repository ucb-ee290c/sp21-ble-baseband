package baseband

import chisel3._
import chisel3.experimental._
import freechips.rocketchip.config.{Config, Field, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._

class WithBLEBasebandModem extends Config((site, up, here) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      implicit val q = p
      implicit val v = implicitly[ValName]
      val blebm = LazyModule(new BLEBasebandModem(OpcodeSet.custom0))
      blebm
    }
  )
})