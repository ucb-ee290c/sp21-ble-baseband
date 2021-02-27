package baseband

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.RoCCCommand

class BasebandConstants extends Bundle {
  val channelIndex = UInt(6.W)
  val crcPreset = UInt(24.W)
  val accessAddress = UInt(32.W)
  val additionalFrameSpace = UInt(32.W)
  val loopbackSelect = UInt(32.W)
}

class Controller(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new RoCCCommand))
    val constants = Output(new BasebandConstants)
  })

  val constants = Reg(new BasebandConstants)
  io.constants := constants

  val s_idle :: s_working :: Nil = Enum(2)
  val state = RegInit(s_idle)

  val cmd = Reg(new RoCCCommand)

  io.cmd.ready := state === s_idle

  switch (state) {
    is (s_idle) {
      when (io.cmd.fire) {
        cmd := io.cmd.bits
        state := s_working
      }
    }
    is (s_working) {
      switch (cmd.inst.funct) {
        is (BasebandISA.CONFIG_CMD) {
          switch (cmd.inst.rs2) {
            is (BasebandISA.CONFIG_CRC_PRESET) {
              constants.crcPreset := cmd.rs1
            }
            is (BasebandISA.CONFIG_ACCESS_ADDRESS) {
              constants.accessAddress := cmd.rs1
            }
            is (BasebandISA.CONFIG_CHANNEL_INDEX) {
              constants.channelIndex := cmd.rs1
            }
            is (BasebandISA.CONFIG_ADDITIONAL_FRAME_SPACE) {
              constants.additionalFrameSpace := cmd.rs1
            }
            is (BasebandISA.CONFIG_LOOPBACK_SELECT) {
              constants.loopbackSelect := cmd.rs1
            }
          }
        }
        is (BasebandISA.SEND_CMD) {

        }
      }
    }
  }
}