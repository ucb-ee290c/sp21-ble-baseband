package baseband

import chisel3.util.Queue
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile._

class BLEBasebandModem(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes = opcodes) {
  val dma = new BasebandDMA

  override lazy val module = new BLEBasebandModemImp(this)
  override val tlNode = dma.id_node
}

class BLEBasebandModemImp(outer: BLEBasebandModem) extends LazyRoCCModuleImp(outer) with HasCoreParameters {
  val interruptServicer = new InterruptServicer
  interruptServicer.io.cmd.in <> io.cmd
  io.resp <> interruptServicer.io.interrupt.resp

  val cmdQueue = Queue(interruptServicer.io.cmd.out, 8) // TODO: should queue depth be a config?

  val controller = new Controller
  controller.io.cmd <> cmdQueue
}
