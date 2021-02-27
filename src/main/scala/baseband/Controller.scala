package baseband

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.tile.RoCCCommand

class Controller(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val cmd = Flipped(Decoupled(new RoCCCommand))
  })

  val channelIndex = Reg(UInt(6.W))
  val crcPreset = Reg(UInt(24.W))
  val accessAddress = Reg(UInt(32.W))
  val additionalFrameSpace = Reg(UInt(32.W))
  val loopbackSelect = Reg(UInt(32.W))

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
              crcPreset := cmd.rs1
            }
            is (BasebandISA.CONFIG_ACCESS_ADDRESS) {
              accessAddress := cmd.rs1
            }
            is (BasebandISA.CONFIG_CHANNEL_INDEX) {
              channelIndex := cmd.rs1
            }
            is (BasebandISA.CONFIG_ADDITIONAL_FRAME_SPACE) {
              additionalFrameSpace := cmd.rs1
            }
            is (BasebandISA.CONFIG_LOOPBACK_SELECT) {
              loopbackSelect := cmd.rs1
            }
          }
        }
        is (BasebandISA.SEND_CMD) {

        }
      }
    }
  }
}