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

class GFSKModem()(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val modemClock = Input(Clock())
    val digital = new GFSKModemDigitalIO
    val analog = new GFSKModemAnalogIO // TODO: Pass through from top level
  })

  withClock(io.modemClock) {
    val tx = new GFSKTX()
    val rx = new GFSKRX()

    tx.io.analog <> io.analog.tx
    rx.io.analog <> io.analog.rx

    val txQueue = new AsyncQueue(UInt(1.W), AsyncQueueParams(depth = 128)) // TODO: Make this a parameterizable value
    txQueue.io.enq_clock := clock
    txQueue.io.enq_reset := reset
    txQueue.io.enq <> io.digital.tx

    txQueue.io.deq_clock := io.modemClock
    txQueue.io.deq_reset := reset
    tx.io.digital.in <> txQueue.io.deq

    val rxQueue = new AsyncQueue(UInt(1.W), AsyncQueueParams(depth = 128)) // TODO: Make this a parameterizable value
    rxQueue.io.enq_clock := io.modemClock
    rxQueue.io.enq_reset := reset
    rxQueue.io.enq <> rx.io.digital.out

    rxQueue.io.deq_clock := clock
    rxQueue.io.deq_reset := reset
    io.digital.rx <> rxQueue.io.deq
  }
}