package baseband

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile._

class BLEBasebandModem(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes = opcodes, nPTWPorts = 0) {
  override lazy val module = new BLEBasebandModemImp(this)
}

class BLEBasebandModemImp(outer: BLEBasebandModem) extends LazyRoCCModuleImp(outer) with HasCoreParameters {

}
