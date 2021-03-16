package modem

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{AsyncQueue, AsyncQueueParams}
import baseband.{BLEBasebandModemParams, BasebandConstants, DecoupledLoopback}
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor


class ModemConstants extends Bundle {
  val LOFSK = Vec(64, UInt(8.W))
  val LOCT = Vec(64, UInt(8.W))
}

class GFSKModemDigitalIO extends Bundle {
  val tx = Flipped(Decoupled(UInt(1.W)))
  val rx = Decoupled(UInt(1.W))
}

class GFSKModemAnalogIO(params: BLEBasebandModemParams) extends Bundle {
  val tx = new AnalogTXIO
  val rx = new AnalogRXIO(params)
  val freqCenter = Output(UInt(8.W))
}

class GFSKModemTuningIO extends Bundle {
  val trim = new Bundle {
    val g0 = UInt(8.W)
    val g1 = UInt(8.W)
    val g2 = UInt(8.W)
    val g3 = UInt(8.W)
    val g4 = UInt(8.W)
    val g5 = UInt(8.W)
    val g6 = UInt(8.W)
    val g7 = UInt(8.W)
  }
  val mixer = new Bundle {
    val r0 = UInt(4.W)
    val r1 = UInt(4.W)
    val r2 = UInt(4.W)
    val r3 = UInt(4.W)
  }
  val i = new Bundle {
    val vgaAtten = UInt(5.W)
    val filter = new Bundle {
      val r0 = UInt(4.W)
      val r1 = UInt(4.W)
      val r2 = UInt(4.W)
      val r3 = UInt(4.W)
      val r4 = UInt(4.W)
      val r5 = UInt(4.W)
      val r6 = UInt(4.W)
      val r7 = UInt(4.W)
      val r8 = UInt(4.W)
      val r9 = UInt(4.W)
    }
  }
  val q = new Bundle {
    val vgaAtten = UInt(5.W)
    val filter = new Bundle {
      val r0 = UInt(4.W)
      val r1 = UInt(4.W)
      val r2 = UInt(4.W)
      val r3 = UInt(4.W)
      val r4 = UInt(4.W)
      val r5 = UInt(4.W)
      val r6 = UInt(4.W)
      val r7 = UInt(4.W)
      val r8 = UInt(4.W)
      val r9 = UInt(4.W)
    }
  }
  val dac = new Bundle {
    val t0 = UInt(6.W)
    val t1 = UInt(6.W)
    val t2 = UInt(6.W)
    val t3 = UInt(6.W)
  }
}

class GFSKModem(params: BLEBasebandModemParams) extends Module {
  val io = IO(new Bundle {
    val digital = new GFSKModemDigitalIO
    val analog = new GFSKModemAnalogIO(params)
    val basebandConstants = Input(new BasebandConstants)
  })

  val constants = RegInit(new ModemConstants, WireInit(new ModemConstants().Lit(
    _.LOCT -> VecInit(Seq.fill(64)(0.U(8.W))),
    _.LOFSK -> VecInit(Seq.fill(64)(0.U(8.W)))

  )))

  io.analog.freqCenter := constants.LOCT(io.basebandConstants.channelIndex)

  val tx = Module(new GFSKTX())
  val rx = Module(new GFSKRX(params))

  val txQueue = Queue(io.digital.tx, params.modemQueueDepth)
  tx.io.digital.in <> txQueue

  val preModemLoopback = Module(new DecoupledLoopback(UInt(1.W)))
  preModemLoopback.io.select := true.B
  preModemLoopback.io.left.in <> txQueue
  tx.io.digital.in <> preModemLoopback.io.left.out
  preModemLoopback.io.right.in <> rx.io.digital.out

  val rxQueue = Queue(preModemLoopback.io.right.out, params.modemQueueDepth)
  io.digital.rx <> rxQueue

  val iQueue = Module(new AsyncQueue(UInt(params.adcBits.W), AsyncQueueParams(depth = params.adcQueueDepth)))
  iQueue.io.enq_clock := io.analog.rx.i.valid.asClock()
  iQueue.io.enq_reset := reset.asBool()
  iQueue.io.deq_clock := clock
  iQueue.io.deq_reset := reset.asBool()

  // TODO: Refactor RX incoming to be ready valid on I and Q bits

  val qQueue = Module(new AsyncQueue(UInt(params.adcBits.W), AsyncQueueParams(depth = params.adcQueueDepth)))
  qQueue.io.enq_clock := io.analog.rx.q.valid.asClock()
  qQueue.io.enq_reset := reset.asBool()
  qQueue.io.deq_clock := clock
  qQueue.io.deq_reset := reset.asBool()

  iQueue.io.enq.bits := io.analog.rx.i.data
  iQueue.io.enq.valid := true.B // TODO: Change this to be based on the modem state = RX
  rx.io.analog.i <> iQueue.io.deq

  qQueue.io.enq.bits := io.analog.rx.q.data
  qQueue.io.enq.valid := true.B // TODO: Change this to be based on the modem state = RX
  rx.io.analog.q <> qQueue.io.deq
}