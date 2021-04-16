package baseband

import chisel3._
import chisel3.util._

class MessageStore(params: BLEBasebandModemParams) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new BLEBasebandModemMessagesIO)
    val out = new BLEBasebandModemMessagesIO
  })

  val errorMessageQ = Module(new Queue(io.in.errorMessage, params.interruptMessageQueueDepth))
  errorMessageQ.io.enq <> io.in.errorMessage
  io.out.errorMessage <> errorMessageQ.io.deq

  val rxFinishMessageQ = Module(new Queue(io.in.rxFinishMessage, params.interruptMessageQueueDepth))
  rxFinishMessageQ.io.enq <> io.in.rxFinishMessage
  io.out.rxFinishMessage <> rxFinishMessageQ.io.deq
}