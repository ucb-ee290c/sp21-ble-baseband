package baseband

import chisel3._
import freechips.rocketchip.config.{Config, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, ValName}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.tile._

class DefaultBasebandConfig extends Config((site, up, here) => {
  case BuildRoCC => up(BuildRoCC) ++ Seq(
    (p: Parameters) => {
      implicit val q = p
      implicit val v = implicitly[ValName]
      LazyModule(new BLEBasebandModem(OpcodeSet.custom0))
    }
  )
  case SystemBusKey => up(SystemBusKey)
})