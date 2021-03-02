package baseband

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.subsystem.SystemBusKey
import freechips.rocketchip.tile._

import ee290cdma._

class BLEBasebandModem(opcodes: OpcodeSet)(implicit p: Parameters) extends LazyRoCC(opcodes = opcodes) {
  val beatBytes = p(SystemBusKey).beatBytes

  val dma = new EE290CDMA(beatBytes, 9, "baseband")

  override lazy val module = new BLEBasebandModemImp(this)
  override val tlNode = dma.id_node
}

class BLEBasebandModemImp(outer: BLEBasebandModem) extends LazyRoCCModuleImp(outer) with HasCoreParameters {
//  val modemIO = IO(new Bundle {
//    val modemClock = Input(Clock())
//    val analog = new GFSKModemAnalogIO
//  })

  import outer.beatBytes

  val interruptServicer = new InterruptServicer
  interruptServicer.io.cmd.in <> io.cmd
  io.resp <> interruptServicer.io.interrupt.resp

  val cmdQueue = Queue(interruptServicer.io.cmd.out, 8) // TODO: should queue depth be a config?

  val controller = new Controller(paddrBits, beatBytes)
  controller.io.cmd <> cmdQueue

  val baseband = new Baseband(paddrBits, beatBytes)
  baseband.io.control <> controller.io.basebandControl

  val basebandLoopback = new DecoupledLoopback(UInt(1.W))
  basebandLoopback.io.select := controller.io.constants.loopbackSelect(0) // TODO: define an object that contains macros for loopback bits
  basebandLoopback.io.left.in <> baseband.io.modem.tx
  baseband.io.modem.rx <> basebandLoopback.io.left.out

//  val modem = new GFSKModem
//  basebandLoopback.io.right.in <> modem.io.baseband.rx
//  modem.io.baseband.tx <> basebandLoopback.io.right.out
//
//  modem.io.analog <> modemIO.analog
//  modem.io.modemClock := modemIO.modemClock
}

