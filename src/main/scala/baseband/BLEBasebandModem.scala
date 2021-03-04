package baseband

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.regmapper.{HasRegMap, RegField, RegisterWriteIO}
import freechips.rocketchip.tilelink.{TLIdentityNode, TLRegBundle, TLRegModule, TLRegisterRouter}

import ee290cdma._

case class BLEBasebandModemParams (
  address: BigInt = 0x8000,
  maxReadSize: Int = 258,
  cmdQueueDepth: Int = 4,
  modemQueueDepth: Int = 128)

case object BLEBasebandModemKey extends Field[Option[BLEBasebandModemParams]](None)

class BLEBasebandModemAnalogIO extends Bundle {
  val modemClock = Input(Clock())
  val offChipMode = Output(Bool())
  val data = new GFSKModemAnalogIO
  val tuning = Output(new GFSKModemTuningIO)
}

class BLEBasebandModemCommand extends Bundle {
  val inst = UInt(32.W)
  val data = UInt(32.W)
}

class BLEBasebandModemBackendIO extends Bundle {
  val cmd = Decoupled(new BLEBasebandModemCommand)
  val interrupt = Input(Bool())
}

trait BLEBasebandModemFrontendBundle extends Bundle {
  val back = new BLEBasebandModemBackendIO
}

trait BLEBasebandModemFrontendModule extends HasRegMap {
  val io: BLEBasebandModemFrontendBundle

  val inst = Wire(new DecoupledIO(UInt(32.W)))
  val data = Reg(UInt(32.W))

  // Writing to the instruction triggers the command to be valid.
  // So if you wish to set data you write that first then write inst
  inst.ready := io.back.cmd.ready
  io.back.cmd.bits.data := data
  io.back.cmd.bits.inst := inst.bits
  io.back.cmd.valid := inst.valid

  interrupts(0) := io.back.interrupt

  regmap(
    0x00 -> Seq(RegField.w(32, inst)),
    0x04 -> Seq(RegField.w(32, data))
  )
}

class BLEBasebandModemFrontend(params: BLEBasebandModemParams, beatBytes: Int)(implicit p: Parameters)
  extends TLRegisterRouter(
    params.address, "baseband", Seq("ucbbar, riscv"),
    beatBytes = beatBytes, interrupts = 1)( // TODO: Interrupts and compatible list
      new TLRegBundle(params, _) with BLEBasebandModemFrontendBundle)(
      new TLRegModule(params, _, _) with BLEBasebandModemFrontendModule)

class BLEBasebandModem(params: BLEBasebandModemParams, beatBytes: Int)(implicit p: Parameters) extends LazyModule {
  val dma = LazyModule(new EE290CDMA(beatBytes, params.maxReadSize, "baseband"))

  val mmio = TLIdentityNode()
  val mem = dma.id_node

  val basebandFrontend = LazyModule(new BLEBasebandModemFrontend(params, beatBytes))
  val intnode = basebandFrontend.intnode

  basebandFrontend.node := mmio

  lazy val module = new BLEBasebandModemImp(params,this)
}

class BLEBasebandModemImp(params: BLEBasebandModemParams, outer: BLEBasebandModem) extends LazyModuleImp(outer) {
  val io = dontTouch(IO(new BLEBasebandModemAnalogIO))

  import outer._

  basebandFrontend.module.io.back.interrupt := false.B

  val cmdQueue = Queue(basebandFrontend.module.io.back.cmd, params.cmdQueueDepth)

  cmdQueue.ready := false.B
}

//class BLEBasebandModem(params: BLEBasebandModemParams)(implicit p: Parameters) extends LazyRoCC(opcodes = opcodes) {
//  val beatBytes = p(SystemBusKey).beatBytes
//
//  val dma = new EE290CDMA(beatBytes, 258, "baseband")
//
//  override lazy val module = new BLEBasebandModemImp(this)
//  val tlNode = dma.id_node
//}
//
//class BLEBasebandModemImp(outer: BLEBasebandModem) extends LazyRoCCModuleImp(outer) with HasCoreParameters {
//  val modemIO = IO(new Bundle {
//    val modemClock = Input(Clock())
//    val analog = new GFSKModemAnalogIO
//  })
//
//  import outer.beatBytes
//
//  val interruptServicer = new InterruptServicer
//  interruptServicer.io.cmd.in <> io.cmd
//  io.resp <> interruptServicer.io.interrupt.resp
//
//  val cmdQueue = Queue(interruptServicer.io.cmd.out, 8) // TODO: should queue depth be a config?
//
//  val controller = new Controller(paddrBits, beatBytes)
//  controller.io.cmd <> cmdQueue
//
//  val baseband = new Baseband(paddrBits, beatBytes)
//  baseband.io.control <> controller.io.basebandControl
//
//  val basebandLoopback = new DecoupledLoopback(UInt(1.W))
//  basebandLoopback.io.select := controller.io.constants.loopbackSelect(0) // TODO: define an object that contains macros for loopback bits
//  basebandLoopback.io.left.in <> baseband.io.modem.tx
//  baseband.io.modem.rx <> basebandLoopback.io.left.out
//
////  val modem = new GFSKModem
////  basebandLoopback.io.right.in <> modem.io.baseband.rx
////  modem.io.baseband.tx <> basebandLoopback.io.right.out
////
////  modem.io.analog <> modemIO.analog
////  modem.io.modemClock := modemIO.modemClock
//}

