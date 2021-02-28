package baseband

import chisel3._
import chisel3.experimental._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}

class BasebandConstants extends Bundle {
  val channelIndex = UInt(6.W)
  val crcSeed = UInt(24.W)
  val whiteningSeed = UInt(7.W)
  val accessAddress = UInt(32.W)
  val additionalFrameSpace = UInt(32.W)
  val loopbackSelect = UInt(32.W)
}

class BasebandIO extends Bundle {
  val constants = Input(new BasebandConstants)
  val assembler = new AssemblerIO
  val disassembler = new DisassemblerIO
}

class Baseband()(implicit p: Parameters) extends LazyModule {
  lazy val module = new LazyModuleImp(this) {
    val io = IO(new BasebandIO)

    // val assembler = new Assembler
    // val disassembler = new Disassembler
  }
}