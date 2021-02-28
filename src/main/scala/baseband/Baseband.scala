package baseband

import chisel3._
import chisel3.experimental._
import chisel3.util._
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

class BasebandIO(addrBits: Int, beatBytes: Int) extends Bundle {
  val control = new Bundle {
    // TODO: Everything needed to control the basebands operation likely:
    //  1. out: disassembler busy (can't TX)
    //  2. out: assembler busy (can't RX or start new TX)
    //  3. in: mode select (if neither is busy which device is active)
  }
  val constants = Input(new BasebandConstants)
  val dma = new Bundle {
    val readResp = Flipped(Decoupled(new BasebandReaderResp))
    val writeReq = Decoupled(new BasebandWriterReq(addrBits, beatBytes))
  }
  val modem = Flipped(new GFSKModemDigitalIO)
}

class Baseband(addrBits: Int, beatBytes: Int)(implicit p: Parameters) extends LazyModule {
  lazy val module = new LazyModuleImp(this) {
    val io = IO(new BasebandIO(addrBits, beatBytes))

    val assembler = new PacketAssembler
    val disassembler = new PacketDisassembler
  }
}