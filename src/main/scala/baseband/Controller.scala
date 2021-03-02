package baseband

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, LazyModuleImpLike}
import freechips.rocketchip.tile.{HasCoreParameters, RoCCCommand, XLen}

import ee290cdma._

class Controller(addrBits: Int, beatBytes: Int)(implicit p: Parameters) extends LazyModule with HasCoreParameters {
  lazy val io = module.io

  lazy val module = new LazyModuleImp(this) {
    val xLen = p(XLen)

    val io = IO(new Bundle {
      val basebandControl = new BasebandControlIO(addrBits)
      val cmd = Flipped(Decoupled(new RoCCCommand))
      val constants = Output(new BasebandConstants) // TODO: Rename? Includes constants that will be used by modem too
      val dma = new Bundle {
        val read = Decoupled(new EE290CDMAReaderReq(addrBits, 9)) // Controller only issues read requests, the baseband issues write requests
      }
      val interrupt = new Bundle {
        val signal = Output(Bool())
        val queue = Decoupled(UInt(xLen.W))
      }
    })

    val constants = RegInit(new BasebandConstants, new BasebandConstants().Lit(
      _.crcSeed -> "b010101010101010101010101".U,
      _.whiteningSeed -> "b1100101".U
    ))

    io.constants := constants

    val s_idle :: s_tx :: s_rx :: s_interrupt :: Nil = Enum(4)
    val state = RegInit(s_idle)

    val cmd = Reg(new RoCCCommand)

    io.cmd.ready := state === s_idle
    io.interrupt.queue.valid := state === s_interrupt

    switch(state) {
      is (s_idle) {
        io.interrupt.signal := false.B
        when(false.B) { // TODO: if the disassembler thinks it is working on a packet we need to switch to RX mode
          state := s_rx
        }
        .elsewhen(io.cmd.fire) {
          switch(io.cmd.bits.inst.funct) {
            is(BasebandISA.CONFIG_CMD) { // Don't need to waste a cycle to setup config
              switch(io.cmd.bits.inst.rs2) {
                is(BasebandISA.CONFIG_CRC_SEED) {
                  constants.crcSeed := cmd.rs1
                }
                is(BasebandISA.CONFIG_WHITENING_SEED) {
                  constants.whiteningSeed := cmd.rs1
                }
                is(BasebandISA.CONFIG_ACCESS_ADDRESS) {
                  constants.accessAddress := cmd.rs1
                }
                is(BasebandISA.CONFIG_CHANNEL_INDEX) {
                  constants.channelIndex := cmd.rs1
                }
                is(BasebandISA.CONFIG_ADDITIONAL_FRAME_SPACE) {
                  constants.additionalFrameSpace := cmd.rs1
                }
                is(BasebandISA.CONFIG_LOOPBACK_SELECT) {
                  constants.loopbackSelect := cmd.rs1
                }
              }
            }
            is(BasebandISA.SEND_CMD) {
              cmd := io.cmd.bits

              // When the disassembler is not working and the modem is not sending out data, we can try to send a new packet
              // We don't check if the assembler is busy / ready becasue it should be covered by the fire in tx state
              when (io.cmd.bits.rs2 > 1.U) {
                //when (~io.basebandControl.disassembler.out.control.busy & ~io.modemControl.rx.out.busy) {
                  io.basebandControl.assembler.in.valid := true.B
                  io.basebandControl.assembler.in.bits.aa := constants.accessAddress
                  io.basebandControl.assembler.in.bits.pduLength := io.cmd.bits.rs2
                  // TODO: switch disassembler off

                  state := s_tx
               // }
              }.otherwise {
                io.interrupt.queue.bits := BasebandISA.INTERRUPT(BasebandISA.INTERRUPT_REASON_INVALID_TX_LENGTH)
                state := s_interrupt
              }


            }
          }
        }
      }
      is (s_tx) {
        when (!io.basebandControl.assembler.out.busy) { // & !io.modemControl.tx.busy) {
          state := s_idle
        }
      }
      is (s_rx) {

      }
      is (s_interrupt) {
        // TODO: When interrupt queue fires to let us take up our message, then we are good to go
        // when (io.interrupt.queue.fire()) {
        //   state := s_idle
        //   io.interrupt.signal := true.B
        // }
      }
    }
  }
}