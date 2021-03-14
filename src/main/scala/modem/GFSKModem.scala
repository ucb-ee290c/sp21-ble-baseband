package modem

import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{AsyncQueue, AsyncQueueParams}

import baseband.BLEBasebandModemParams

class GFSKModemDigitalIO extends Bundle {
  val tx = Flipped(Decoupled(UInt(1.W)))
  val rx = Decoupled(UInt(1.W))
}

class GFSKModemAnalogIO(params: BLEBasebandModemParams) extends Bundle {
  val tx = new AnalogTXIO
  val rx = new AnalogRXIO(params)
  val freqCenter = Output(UInt(6.W))
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
  val dac =  new Bundle {
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
  })

  val tx = Module(new GFSKTX())
  val rx = Module(new GFSKRX(params))

  val txQueue = Queue(io.digital.tx, params.modemQueueDepth)
  tx.io.digital.in <> txQueue

  val rxQueue = Queue(rx.io.digital.out, params.modemQueueDepth)
  io.digital.rx <> rxQueue

  val iQueue = Module(new AsyncQueue(UInt(params.adcBits.W), AsyncQueueParams(depth = params.adcQueueDepth)))
  iQueue.io.enq_clock := io.analog.rx.i.valid.asClock()
  iQueue.io.enq_reset := reset
  iQueue.io.deq_clock := clock
  iQueue.io.deq_reset := reset

  // TODO: Refactor RX incoming to be ready valid on I and Q bits

  val qQueue = Module(new AsyncQueue(UInt(params.adcBits.W), AsyncQueueParams(depth = params.adcQueueDepth)))
  qQueue.io.enq_clock := io.analog.rx.q.valid.asClock()
  qQueue.io.enq_reset := reset
  qQueue.io.deq_clock := clock
  qQueue.io.deq_reset := reset

  iQueue.io.enq.bits := io.analog.rx.i.data
  iQueue.io.enq.valid := true.B // TODO: Change this to be based on the modem state = RX

  qQueue.io.enq.bits := io.analog.rx.q.data
  qQueue.io.enq.valid := true.B // TODO: Change this to be based on the modem state = RX
}