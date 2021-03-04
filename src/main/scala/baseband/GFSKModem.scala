package baseband

import chisel3._
import chisel3.util.Decoupled
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.util.{AsyncQueue, AsyncQueueParams}

class GFSKModemDigitalIO extends Bundle {
  val tx = Flipped(Decoupled(UInt(1.W)))
  val rx = Decoupled(UInt(1.W))
}

class GFSKModemAnalogIO extends Bundle {
  val tx = new AnalogTXIO
  val rx = new AnalogRXIO
}

class GFSKModemTuningIO extends Bundle {
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
}

class GFSKModem()(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val modemClock = Input(Clock())
    val digital = new GFSKModemDigitalIO
    val analog = new GFSKModemAnalogIO // TODO: Pass through from top level
  })

  // The queues must be wired to clock and io.modemClock outside of the withClock section, else clock = io.modemClock
  val txQueue = new AsyncQueue(UInt(1.W), AsyncQueueParams(depth = 128)) // TODO: Make this a parameterizable value
  txQueue.io.enq_clock := clock
  txQueue.io.enq_reset := reset
  txQueue.io.deq_clock := io.modemClock
  txQueue.io.deq_reset := reset

  val rxQueue = new AsyncQueue(UInt(1.W), AsyncQueueParams(depth = 128)) // TODO: Make this a parameterizable value
  rxQueue.io.enq_clock := io.modemClock
  rxQueue.io.enq_reset := reset
  rxQueue.io.deq_clock := clock
  rxQueue.io.deq_reset := reset

  withClock(io.modemClock) {
    val tx = new GFSKTX()
    val rx = new GFSKRX()

    tx.io.analog <> io.analog.tx
    rx.io.analog <> io.analog.rx

    txQueue.io.enq <> io.digital.tx
    tx.io.digital.in <> txQueue.io.deq

    rxQueue.io.enq <> rx.io.digital.out
    io.digital.rx <> rxQueue.io.deq
  }
}