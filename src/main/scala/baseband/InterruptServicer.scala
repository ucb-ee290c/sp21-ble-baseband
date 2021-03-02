package baseband

import chisel3._
import chisel3.util.{Decoupled, Enum, Queue}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.{RoCCCommand, RoCCResponse, XLen}


class InterruptServicer()(implicit p: Parameters) extends Module {
  val xLen = p(XLen)
  val io = IO(new Bundle {
    val interrupt = new Bundle {
      val in = Flipped(Decoupled(UInt(xLen.W)))
      val resp = Decoupled(new RoCCResponse)
    }
    val cmd = new Bundle {
      val in = Flipped(Decoupled(new RoCCCommand))
      val out = Decoupled(new RoCCCommand)
    }
  })

  val cmd = Reg(new RoCCCommand)

  val s_idle :: s_queue :: s_resp :: s_pass :: Nil = Enum(4)
  val state = RegInit(s_idle)

  io.cmd.in.ready := io.cmd.out.ready & (state === s_idle)

  io.cmd.out.valid := state === s_pass
  io.cmd.out.bits := cmd

  val interruptQueue = Queue(io.interrupt.in)
  interruptQueue.ready := state === s_queue

  io.interrupt.resp.bits.data := interruptQueue.bits
  io.interrupt.resp.bits.data := cmd.inst.rd
  io.interrupt.resp.valid := state === s_resp

  when (io.cmd.in.fire) {
    cmd := io.cmd.in.bits
    state := Mux(io.cmd.in.bits.inst.funct === BasebandISA.INTERRUPT_CMD, s_queue, s_pass)
  }

  when (interruptQueue.fire) {
    state := s_resp
  }

  when (io.cmd.out.fire | io.interrupt.resp.fire) {
    state := s_idle
  }
}