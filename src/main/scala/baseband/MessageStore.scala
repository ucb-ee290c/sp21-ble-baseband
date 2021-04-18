package baseband

import chisel3._
import chisel3.util._

class MessageStore(params: BLEBasebandModemParams) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new BLEBasebandModemMessagesIO)
    val out = new BLEBasebandModemMessagesIO
  })

  val rxErrorMessageQ = Queue(io.in.rxErrorMessage, params.interruptMessageQueueDepth)
  io.out.rxErrorMessage <> rxErrorMessageQ

  val rxFinishMessageQ = Queue(io.in.rxFinishMessage, params.interruptMessageQueueDepth)
  io.out.rxFinishMessage <> rxFinishMessageQ

  val txErrorMessageQ = Queue(io.in.txErrorMessage, params.interruptMessageQueueDepth)
  io.out.txErrorMessage <> txErrorMessageQ
}