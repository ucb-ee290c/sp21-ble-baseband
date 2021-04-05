package baseband

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import freechips.rocketchip.regmapper.{HasRegMap, RegField, RegisterWriteIO}
import freechips.rocketchip.tilelink.{TLIdentityNode, TLRegBundle, TLRegModule, TLRegisterRouter}

import ee290cdma._
import modem._

case class BLEBasebandModemParams (
  address: BigInt = 0x8000,
  paddrBits: Int = 32,
  maxReadSize: Int = 258,
  adcBits: Int = 8,
  adcQueueDepth: Int = 2,
  cmdQueueDepth: Int = 4,
  modemQueueDepth: Int = 128,
  cyclesPerSymbol: Int = 10,
  agcMaxWindow: Int = 5)

case object BLEBasebandModemKey extends Field[Option[BLEBasebandModemParams]](None)

class BLEBasebandModemAnalogIO(params: BLEBasebandModemParams) extends Bundle {
  val offChipMode = Output(Bool())
  val data = new modem.GFSKModemAnalogIO(params)
  val tuning = Output(new modem.GFSKModemTuningIO)
}

class BLEBasebandModemCommand extends Bundle {
  val inst = new Bundle {
    val primaryInst = UInt(4.W)
    val secondaryInst = UInt(4.W)
    val data = UInt(24.W)
  }
  val additionalData = UInt(32.W)
}

class BLEBasebandModemStatus extends Bundle {
  val status0 = UInt(32.W)
  val status1 = UInt(32.W)
  val status2 = UInt(32.W)
  val status3 = UInt(32.W)
  val status4 = UInt(32.W)
}

class BLEBasebandModemBackendIO extends Bundle {
  val cmd = Decoupled(new BLEBasebandModemCommand)
  val lutCmd = Decoupled(new GFSKModemLUTCommand)
  val status = Input(new BLEBasebandModemStatus)
  val interrupt = Input(Bool())
}

trait BLEBasebandModemFrontendBundle extends Bundle {
  val params: BLEBasebandModemParams

  val back = new BLEBasebandModemBackendIO
  val tuning = Output(new GFSKModemTuningIO)
  val tuningControl = Output(new GFSKModemTuningControlIO(params))
}

trait BLEBasebandModemFrontendModule extends HasRegMap {
  val params: BLEBasebandModemParams

  val io: BLEBasebandModemFrontendBundle

  // Assertions for RegMap Parameter Correctness
  assert(params.adcBits <= 8, s"ADC bits set to ${params.adcBits}, must than or equal to 8")

  // Instruction from processor
  val inst = Wire(new DecoupledIO(UInt(32.W)))
  val additionalData = Reg(UInt(32.W))

  // Writing to the instruction triggers the command to be valid.
  // So if you wish to set data you write that first then write inst
  inst.ready := io.back.cmd.ready
  io.back.cmd.bits.additionalData := additionalData
  io.back.cmd.bits.inst.primaryInst := inst.bits(3, 0)
  io.back.cmd.bits.inst.secondaryInst := inst.bits(7, 4)
  io.back.cmd.bits.inst.data := inst.bits(31, 8)
  io.back.cmd.valid := inst.valid

  // LUT set instruction from processor
  val lutCmd = Wire(new DecoupledIO(UInt(32.W)))

  lutCmd.ready := io.back.lutCmd.ready
  io.back.lutCmd.bits.lut := lutCmd.bits(3, 0)
  io.back.lutCmd.bits.address := lutCmd.bits(9, 4)
  io.back.lutCmd.bits.value := lutCmd.bits(31, 10)
  io.back.lutCmd.valid := lutCmd.valid

  // Status regs
  val status0 = RegInit(0.U(32.W))
  val status1 = RegInit(0.U(32.W))
  val status2 = RegInit(0.U(32.W))
  val status3 = RegInit(0.U(32.W))
  val status4 = RegInit(0.U(32.W))

  status0 := io.back.status.status0
  status1 := io.back.status.status1
  status2 := io.back.status.status2
  status3 := io.back.status.status3
  status4 := io.back.status.status4

  // Tuning bits store
  val trim_g0 = RegInit(0.U(8.W))
  val trim_g1 = RegInit(0.U(8.W))
  val trim_g2 = RegInit(0.U(8.W))
  val trim_g3 = RegInit(0.U(8.W))
  val trim_g4 = RegInit(0.U(8.W))
  val trim_g5 = RegInit(0.U(8.W))
  val trim_g6 = RegInit(0.U(8.W))
  val trim_g7 = RegInit(0.U(8.W))

  val mixer_r0 = RegInit(0.U(4.W))
  val mixer_r1 = RegInit(0.U(4.W))
  val mixer_r2 = RegInit(0.U(4.W))
  val mixer_r3 = RegInit(0.U(4.W))

  val i_vgaAtten = RegInit(0.U(10.W))
  val i_vgaAtten_reset = RegInit(false.B)
  val i_vgaAtten_useAGC = RegInit(false.B)
  val i_vgaAtten_sampleWindow = RegInit(1.U(log2Ceil(params.agcMaxWindow).W))
  val i_vgaAtten_idealPeakToPeak = RegInit(math.pow(2, params.adcBits - 1).toInt.U(params.adcBits.W)) // TODO: Verify initial value
  val i_vgaAtten_gain = RegInit(32.U(8.W)) // TODO: Verify initial value

  val i_filter_r0 = RegInit(0.U(4.W))
  val i_filter_r1 = RegInit(0.U(4.W))
  val i_filter_r2 = RegInit(0.U(4.W))
  val i_filter_r3 = RegInit(0.U(4.W))
  val i_filter_r4 = RegInit(0.U(4.W))
  val i_filter_r5 = RegInit(0.U(4.W))
  val i_filter_r6 = RegInit(0.U(4.W))
  val i_filter_r7 = RegInit(0.U(4.W))
  val i_filter_r8 = RegInit(0.U(4.W))
  val i_filter_r9 = RegInit(0.U(4.W))

  val q_vgaAtten = RegInit(0.U(10.W))
  val q_vgaAtten_reset = RegInit(false.B)
  val q_vgaAtten_useAGC = RegInit(false.B)
  val q_vgaAtten_sampleWindow = RegInit(1.U(log2Ceil(params.agcMaxWindow).W))
  val q_vgaAtten_idealPeakToPeak = RegInit(128.U(params.adcBits.W)) // TODO: Verify initial value
  val q_vgaAtten_gain = RegInit(32.U(8.W)) // TODO: Verify initial value

  val q_filter_r0 = RegInit(0.U(4.W))
  val q_filter_r1 = RegInit(0.U(4.W))
  val q_filter_r2 = RegInit(0.U(4.W))
  val q_filter_r3 = RegInit(0.U(4.W))
  val q_filter_r4 = RegInit(0.U(4.W))
  val q_filter_r5 = RegInit(0.U(4.W))
  val q_filter_r6 = RegInit(0.U(4.W))
  val q_filter_r7 = RegInit(0.U(4.W))
  val q_filter_r8 = RegInit(0.U(4.W))
  val q_filter_r9 = RegInit(0.U(4.W))

  val dac_t0 = RegInit(0.U(6.W))
  val dac_t1 = RegInit(0.U(6.W))
  val dac_t2 = RegInit(0.U(6.W))
  val dac_t3 = RegInit(0.U(6.W))

  val enableDebug = RegInit(false.B)
  val mux_dbg_in = RegInit(0.U(10.W))
  val mux_dbg_out = RegInit(0.U(10.W))
  val enable_rx_i = RegInit(0.U(5.W))
  val enable_rx_q = RegInit(0.U(5.W))

  io.tuning.trim.g0 := trim_g0
  io.tuning.trim.g1 := trim_g1
  io.tuning.trim.g2 := trim_g2
  io.tuning.trim.g3 := trim_g3
  io.tuning.trim.g4 := trim_g4
  io.tuning.trim.g5 := trim_g5
  io.tuning.trim.g6 := trim_g6
  io.tuning.trim.g7 := trim_g7

  io.tuning.mixer.r0 := mixer_r0
  io.tuning.mixer.r1 := mixer_r1
  io.tuning.mixer.r2 := mixer_r2
  io.tuning.mixer.r3 := mixer_r3

  io.tuning.i.vgaAtten := i_vgaAtten
  io.tuningControl.i.AGC.useAGC := i_vgaAtten_useAGC
  io.tuningControl.i.AGC.control.reset := i_vgaAtten_reset
  io.tuningControl.i.AGC.control.sampleWindow := i_vgaAtten_sampleWindow
  io.tuningControl.i.AGC.control.idealPeakToPeak := i_vgaAtten_idealPeakToPeak
  io.tuningControl.i.AGC.control.gain := i_vgaAtten_gain.asFixedPoint(6.BP)

  io.tuning.i.filter.r0 := i_filter_r0
  io.tuning.i.filter.r0 := i_filter_r1
  io.tuning.i.filter.r0 := i_filter_r2
  io.tuning.i.filter.r0 := i_filter_r3
  io.tuning.i.filter.r0 := i_filter_r4
  io.tuning.i.filter.r0 := i_filter_r5
  io.tuning.i.filter.r0 := i_filter_r6
  io.tuning.i.filter.r0 := i_filter_r7
  io.tuning.i.filter.r0 := i_filter_r8
  io.tuning.i.filter.r0 := i_filter_r9

  io.tuning.q.vgaAtten := q_vgaAtten
  io.tuningControl.q.AGC.useAGC := q_vgaAtten_useAGC
  io.tuningControl.q.AGC.control.reset := q_vgaAtten_reset
  io.tuningControl.q.AGC.control.sampleWindow := q_vgaAtten_sampleWindow
  io.tuningControl.q.AGC.control.idealPeakToPeak := q_vgaAtten_idealPeakToPeak
  io.tuningControl.q.AGC.control.gain := q_vgaAtten_gain.asFixedPoint(6.BP)

  io.tuning.q.filter.r0 := q_filter_r0
  io.tuning.q.filter.r0 := q_filter_r1
  io.tuning.q.filter.r0 := q_filter_r2
  io.tuning.q.filter.r0 := q_filter_r3
  io.tuning.q.filter.r0 := q_filter_r4
  io.tuning.q.filter.r0 := q_filter_r5
  io.tuning.q.filter.r0 := q_filter_r6
  io.tuning.q.filter.r0 := q_filter_r7
  io.tuning.q.filter.r0 := q_filter_r8
  io.tuning.q.filter.r0 := q_filter_r9

  io.tuning.dac.t0 := dac_t0
  io.tuning.dac.t1 := dac_t1
  io.tuning.dac.t2 := dac_t2
  io.tuning.dac.t3 := dac_t3

  io.tuningControl.debug.enabled := enableDebug
  io.tuning.mux.dbg.in := mux_dbg_in
  io.tuning.mux.dbg.out := mux_dbg_out
  io.tuning.enable.rx.i := enable_rx_i
  io.tuning.enable.rx.q := enable_rx_q


  interrupts(0) := io.back.interrupt

  regmap(
    0x00 -> Seq(RegField.w(32, inst)), // Command start
    0x04 -> Seq(RegField.w(32, additionalData)),
    0x08 -> Seq(RegField.r(32, status0)), // Status start
    0x0C -> Seq(RegField.r(32, status1)),
    0x10 -> Seq(RegField.r(32, status2)),
    0x14 -> Seq(RegField.r(32, status3)),
    0x18 -> Seq(RegField.r(32, status4)),
    0x1C -> Seq(RegField(8, trim_g0)), // Tuning start, Trim
    0x1D -> Seq(RegField(8, trim_g1)),
    0x1E -> Seq(RegField(8, trim_g2)),
    0x1F -> Seq(RegField(8, trim_g3)),
    0x20 -> Seq(RegField(8, trim_g4)),
    0x21 -> Seq(RegField(8, trim_g5)),
    0x22 -> Seq(RegField(8, trim_g6)),
    0x23 -> Seq(RegField(8, trim_g7)),
    0x24 -> Seq( // Mixer
      RegField(4, mixer_r0),
      RegField(4, mixer_r1)),
    0x25 -> Seq(
      RegField(4, mixer_r2),
      RegField(4, mixer_r3)),
    0x26 -> Seq(RegField(10, i_vgaAtten)), // I VGA
    0x28 -> Seq(RegField(1, i_vgaAtten_reset)),
    0x29 -> Seq(RegField(1, i_vgaAtten_useAGC)),
    0x2A -> Seq(RegField(log2Ceil(params.agcMaxWindow), i_vgaAtten_sampleWindow)),
    0x2B -> Seq(RegField(params.adcBits, i_vgaAtten_idealPeakToPeak)),
    0x2C -> Seq(RegField(8, i_vgaAtten_gain)),
    0x2D -> Seq( // I Filter
      RegField(4, i_filter_r0),
      RegField(4, i_filter_r1)),
    0x2E -> Seq(
      RegField(4, i_filter_r2),
      RegField(4, i_filter_r3)),
    0x2F -> Seq(
      RegField(4, i_filter_r4),
      RegField(4, i_filter_r5)),
    0x30 -> Seq(
      RegField(4, i_filter_r6),
      RegField(4, i_filter_r7)),
    0x31 -> Seq(
      RegField(4, i_filter_r8),
      RegField(4, i_filter_r9)),
    0x32 -> Seq(RegField(10, q_vgaAtten)), // Q VGA
    0x34 -> Seq(RegField(1, q_vgaAtten_reset)),
    0x35 -> Seq(RegField(1, q_vgaAtten_useAGC)),
    0x36 -> Seq(RegField(log2Ceil(params.agcMaxWindow), q_vgaAtten_sampleWindow)),
    0x37 -> Seq(RegField(params.adcBits, q_vgaAtten_idealPeakToPeak)),
    0x38 -> Seq(RegField(8, q_vgaAtten_gain)),
    0x39 -> Seq( // Q Filter
      RegField(4, q_filter_r0),
      RegField(4, q_filter_r1)),
    0x3A -> Seq(
      RegField(4, q_filter_r2),
      RegField(4, q_filter_r3)),
    0x3B -> Seq(
      RegField(4, q_filter_r4),
      RegField(4, q_filter_r5)),
    0x3C -> Seq(
      RegField(4, q_filter_r6),
      RegField(4, q_filter_r7)),
    0x3D -> Seq(
      RegField(4, q_filter_r8),
      RegField(4, q_filter_r9)),
    0x3E -> Seq(RegField(6, dac_t0)), // DAC
    0x3F -> Seq(RegField(6, dac_t1)),
    0x40 -> Seq(RegField(6, dac_t2)),
    0x41 -> Seq(RegField(6, dac_t3)),
    0x42 -> Seq(RegField(1, enableDebug)), // Debug Configuration
    0x43 -> Seq(RegField(10, mux_dbg_in)),
    0x45 -> Seq(RegField(10, mux_dbg_out)),
    0x47 -> Seq(RegField(5, enable_rx_i)), // Manual enable values
    0x48 -> Seq(RegField(5, enable_rx_q)),
    0x49 -> Seq(RegField.w(32, lutCmd)) // LUT Programming
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

  lazy val module = new BLEBasebandModemImp(params, beatBytes,this)
}

class BLEBasebandModemImp(params: BLEBasebandModemParams, beatBytes: Int, outer: BLEBasebandModem)(implicit p: Parameters) extends LazyModuleImp(outer) {
  val io = dontTouch(IO(new BLEBasebandModemAnalogIO(params)))

  val basebandFrontend = outer.basebandFrontend.module
  val dma = outer.dma.module

  basebandFrontend.io.back.interrupt := false.B

  val cmdQueue = Queue(basebandFrontend.io.back.cmd, params.cmdQueueDepth)

  val controller = Module(new Controller(params, beatBytes))
  controller.io.cmd <> cmdQueue
  controller.io.dma.readReq <> dma.io.read.req
  controller.io.dma.readResp <> dma.io.read.resp

  val baseband = Module(new Baseband(params, beatBytes))
  baseband.io.control <> controller.io.basebandControl
  baseband.io.constants := controller.io.constants
  baseband.io.dma.readData <> dma.io.read.queue
  baseband.io.dma.writeReq <> dma.io.write.req

  val modem = Module(new GFSKModem(params))
  modem.io.lutCmd <> basebandFrontend.io.back.lutCmd
  modem.io.constants := controller.io.constants
  modem.io.digital.tx <> baseband.io.modem.tx
  modem.io.digital.rx <> baseband.io.modem.rx
  modem.io.analog.rx <> io.data.rx
  modem.io.tuning.control := basebandFrontend.io.tuningControl
  modem.io.analog.tx.pllReady := io.data.tx.pllReady

  // Other off chip / analog IO
  io.tuning.trim := basebandFrontend.io.tuning.trim
  io.tuning.mixer := basebandFrontend.io.tuning.mixer
  io.tuning.i.filter := basebandFrontend.io.tuning.i.filter
  io.tuning.q.filter := basebandFrontend.io.tuning.q.filter
  io.tuning.i.vgaAtten := Mux(basebandFrontend.io.tuningControl.i.AGC.useAGC, modem.io.tuning.data.i.vgaAtten, basebandFrontend.io.tuning.i.vgaAtten)
  io.tuning.q.vgaAtten := Mux(basebandFrontend.io.tuningControl.q.AGC.useAGC, modem.io.tuning.data.q.vgaAtten, basebandFrontend.io.tuning.q.vgaAtten)
  io.tuning.enable.rx.i := Mux(basebandFrontend.io.tuningControl.debug.enabled, basebandFrontend.io.tuning.enable.rx.i, controller.io.analog.enable.rx)
  io.tuning.enable.rx.q := Mux(basebandFrontend.io.tuningControl.debug.enabled, basebandFrontend.io.tuning.enable.rx.q, controller.io.analog.enable.rx)

  io.data.pllD := controller.io.analog.pllD
  io.data.loCT := modem.io.analog.loCT
  io.data.tx.loFSK := modem.io.analog.tx.loFSK
}
