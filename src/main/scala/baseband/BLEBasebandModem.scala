package baseband

import chisel3._
import chisel3.experimental._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.subsystem.SystemBusKey
import freechips.rocketchip.tile._

class BLEBasebandModem(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes = opcodes) {
  val beatBytes = p(SystemBusKey).beatBytes

  val dma = new BasebandDMA(beatBytes)

  override lazy val module = new BLEBasebandModemImp(this)
  override val tlNode = dma.id_node
}

class BLEBasebandModemImp(outer: BLEBasebandModem) extends LazyRoCCModuleImp(outer) with HasCoreParameters {
  val modemIO = IO(new Bundle {
    val modemClock = Input(Clock())
    val analog = new GFSKModemAnalogIO
  })

  import outer.beatBytes

  val interruptServicer = new InterruptServicer
  interruptServicer.io.cmd.in <> io.cmd
  io.resp <> interruptServicer.io.interrupt.resp

  val cmdQueue = Queue(interruptServicer.io.cmd.out, 8) // TODO: should queue depth be a config?

  val controller = new Controller(paddrBits, beatBytes)
  controller.module.io.cmd <> cmdQueue

  val baseband = new Baseband(paddrBits, beatBytes)
  val modem = new GFSKModem

  modem.io.analog <> modemIO.analog
  modem.io.modemClock := modemIO.modemClock
}

