package baseband

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp, LazyModuleImpLike}
import freechips.rocketchip.tile.{HasCoreParameters, RoCCCommand, XLen}

import ee290cdma._

class Controller(addrBits: Int, beatBytes: Int) extends Module {
  val io = IO(new Bundle {
    val basebandControl = Flipped(new BasebandControlIO(addrBits))
    val cmd = Flipped(Decoupled(new BLEBasebandModemCommand))
    val constants = Output(new BasebandConstants) // TODO: Rename? Includes constants that will be used by modem too
    val dma = new Bundle {
      val read = Decoupled(new EE290CDMAReaderReq(addrBits, 258)) // Controller only issues read requests, the baseband issues write requests
    }
  })

  val constants = RegInit(new BasebandConstants, WireInit(new BasebandConstants().Lit(
    _.crcSeed -> "x555555".U,
    _.channelIndex -> "b010011".U,
    _.accessAddress -> "x8E89BED6".U
  )))

  io.constants := constants

  val s_idle :: s_tx_waiting :: s_tx_active :: s_rx_waiting :: s_rx_active :: s_debug_waiting :: s_debug_active :: s_interrupt :: Nil = Enum(8)

  val state = RegInit(s_idle)

  val cmd = Reg(new BLEBasebandModemCommand)

  // Baseband control wires
  io.basebandControl.assembler.in.bits.aa := constants.accessAddress
  io.basebandControl.assembler.in.bits.pduLength := io.cmd.bits.inst.data - 2.U
  io.basebandControl.assembler.in.valid := state === s_tx_waiting | state === s_debug_waiting

  io.basebandControl.disassembler.in.bits.aa := constants.accessAddress
  io.basebandControl.disassembler.in.valid := state === s_rx_waiting | state === s_debug_waiting

  // Command wires
  io.cmd.ready := state === s_idle

  switch(state) {
    is (s_idle) {
      when (io.cmd.fire) {
        switch (io.cmd.bits.inst.primaryInst) {
          is (BasebandISA.CONFIG_CMD) { // Don't need to waste a cycle to setup config
            switch (io.cmd.bits.inst.secondaryInst) {
              is (BasebandISA.CONFIG_CRC_SEED) {
                constants.crcSeed := io.cmd.bits.additionalData(23, 0)
              }
              is (BasebandISA.CONFIG_ACCESS_ADDRESS) {
                constants.accessAddress := io.cmd.bits.additionalData
              }
              is (BasebandISA.CONFIG_CHANNEL_INDEX) {
                constants.channelIndex := io.cmd.bits.additionalData(5, 0)
              }
              is (BasebandISA.CONFIG_ADDITIONAL_FRAME_SPACE) {
                constants.additionalFrameSpace := io.cmd.bits.additionalData
              }
              is (BasebandISA.CONFIG_LOOPBACK_SELECT) {
                constants.loopbackSelect := io.cmd.bits.additionalData
              }
            }
          }
          is (BasebandISA.SEND_CMD) {
            cmd := io.cmd.bits

            when (io.cmd.bits.inst.data > 1.U & io.cmd.bits.inst.data < 258.U) {

              // TODO: Switch off chip mode
              state := s_tx_waiting
            }.otherwise {
              // TODO: Send interrupt for illegal PDU width
            }
          }
          is (BasebandISA.RECEIVE_CMD) {
            cmd := io.cmd.bits

            // TODO: Check if legal address for write?
            // TODO: Switch off chip mode
            state := s_rx_waiting
          }
        }
      }
    }
    is (s_tx_waiting) {
      when (io.basebandControl.assembler.in.fire()) {
        state := s_tx_active
      }
    }
    is (s_tx_active) {
      when (io.basebandControl.assembler.out.done) {
        state := s_idle // TODO: Need waiting on modem state
      }
    }
    is (s_rx_waiting) {
      when(io.basebandControl.disassembler.in.fire()) {
        state := s_rx_active
      }
    }
    is (s_rx_active) {
      when (io.basebandControl.disassembler.out.done) {
        state := s_idle
      }
    }
    is (s_debug_waiting) {

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