package baseband

import chisel3._
import chisel3.util._
import freechips.rocketchip.config.Parameters

import ee290cdma._

class BasebandConstants extends Bundle {
  val channelIndex = UInt(6.W)
  val crcSeed = UInt(24.W)
  val whiteningSeed = UInt(7.W)
  val accessAddress = UInt(32.W)
  val additionalFrameSpace = UInt(32.W)
  val loopbackSelect = UInt(32.W)
}

class BasebandDMAIO(addrBits: Int, beatBytes: Int) extends Bundle {
  val readResp = Flipped(Decoupled(new EE290CDMAReaderResp(9)))
  val writeReq = Decoupled(new EE290CDMAWriterReq(addrBits, beatBytes))
}

class BasebandControlIO(addrBits: Int) extends Bundle {
  val assembler = new Bundle {
    val in = Flipped(Decoupled(new PAControlInputBundle))
    val out = Output(new PAControlOutputBundle)
  }
}

class BasebandIO(addrBits: Int, beatBytes: Int) extends Bundle {
  val constants = Input(new BasebandConstants)
  val control = new BasebandControlIO(addrBits)
  val dma = new BasebandDMAIO(addrBits, beatBytes)
  val modem = Flipped(new GFSKModemDigitalIO)
}

class Baseband(addrBits: Int, beatBytes: Int)(implicit p: Parameters) extends Module {
  val io = IO(new BasebandIO(addrBits, beatBytes))

  val dmaPacketDisassembler = new DMAPacketDisassembler(beatBytes)
  val assembler = new PacketAssembler
  dmaPacketDisassembler.io.out.done := assembler.io.out.control.done
  assembler.io.out.data.bits := dmaPacketDisassembler.io.out.data


  val dmaPacketAssembler = new DMAPacketAssembler(beatBytes)
  val disassembler = new PacketDisassembler
  dmaPacketAssembler.io.in.done := disassembler.io.out.bits.done
}