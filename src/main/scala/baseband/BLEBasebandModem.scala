package baseband

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile._

class BLEBasebandModem(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes = opcodes, nPTWPorts = 0) {
  val dma = new BasebandDMA

  override lazy val module = new BLEBasebandModemImp(this)
  override val tlNode = dma.id_node
}

class BLEBasebandModemImp(outer: BLEBasebandModem) extends LazyRoCCModuleImp(outer) with HasCoreParameters {

}
