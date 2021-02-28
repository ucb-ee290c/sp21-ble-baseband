package baseband

import freechips.rocketchip.config.{Config, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
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