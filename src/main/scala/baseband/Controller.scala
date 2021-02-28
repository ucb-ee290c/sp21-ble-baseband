package baseband

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, LazyModuleImpLike}
import freechips.rocketchip.tile.{HasCoreParameters, RoCCCommand}

class Controller(addrBits: Int, beatBytes: Int)(implicit p: Parameters) extends LazyModule with HasCoreParameters {
  lazy val module = new LazyModuleImp(this) {
    val io = IO(new Bundle {
      val cmd = Flipped(Decoupled(new RoCCCommand))
      val baseband = new BasebandIO(addrBits, beatBytes)
      val dma = new Bundle {
        val read = Decoupled(new BasebandReaderReq(addrBits)) // Controller only issues read requests, the baseband issues write requests
      }
    })

    val constants = RegInit(new BasebandConstants, new BasebandConstants().Lit(
      _.crcSeed -> "b010101010101010101010101".U,
      _.whiteningSeed -> "b1100101".U
    ))
    io.baseband.constants := constants

    val s_idle :: s_tx :: s_rx :: Nil = Enum(3)
    val state = RegInit(s_idle)

    val cmd = Reg(new RoCCCommand)

    io.cmd.ready := state === s_idle

    switch(state) {
      is(s_idle) {
        when(io.baseband.disassembler.busy) { // TODO: if the disassembler thinks it is working on a packet we need to switch to RX mode
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
                // TODO: Setup things for first cycle of send command and go into working state
                cmd := io.cmd.bits
                state := s_tx
              }
            }
          }
      }
      is(s_tx) {

      }
      is(s_rx) {

      }
    }
  }
}