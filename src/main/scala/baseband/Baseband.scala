package baseband

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config.Parameters

import ee290cdma._
import modem._

class BasebandConstants extends Bundle {
  val channelIndex = UInt(6.W)
  val crcSeed = UInt(24.W)
  val accessAddress = UInt(32.W)
}

class BasebandDMAIO(addrBits: Int, beatBytes: Int) extends Bundle {
  val readData = Flipped(Decoupled(UInt((beatBytes * 8).W)))
  val writeReq = Decoupled(new EE290CDMAWriterReq(addrBits, beatBytes))
}

class AssemblerControlIO extends Bundle {
  val in = Flipped(Decoupled(new PAControlInputBundle))
  val out = Output(new PAControlOutputBundle)
}

class DisassemblerControlIO extends Bundle {
  val in = Flipped(Decoupled(new PDAControlInputBundle))
  val out = Output(new PDAControlOutputBundle)
}

class BasebandControlIO(val addrBits: Int) extends Bundle {
  val assembler = new AssemblerControlIO
  val disassembler = new DisassemblerControlIO
  val baseAddr = Flipped(Valid(UInt(addrBits.W)))
  val loopback = Input(Vec(2, Bool()))
}

class BasebandDMAAddresser(addrBits: Int, beatBytes: Int) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new DMAPacketAssemblerDMAOUTIO(beatBytes)))
    val out = Decoupled(new EE290CDMAWriterReq(addrBits, beatBytes))
    val baseAddr = Flipped(Valid(UInt(addrBits.W)))
  })

  val out_valid = RegInit(false.B)
  val out_ready = RegInit(false.B)
  val out_totalBytes = Reg(UInt(log2Ceil(beatBytes + 1).W))
  val out_data = Reg(UInt((8 * beatBytes).W))
  val out_addr = Reg(UInt(addrBits.W))

  val offset = RegInit(0.U(addrBits.W))

  val baseAddr = Reg(UInt(addrBits.W))

  io.out.valid := out_valid
  io.out.bits.totalBytes := out_totalBytes
  io.out.bits.data := out_data
  io.out.bits.addr := out_addr
  out_ready := io.out.ready

  io.in.ready := out_ready

  when (io.baseAddr.fire()) {
    baseAddr := io.baseAddr.bits
    offset := 0.U
  } .elsewhen(io.in.fire()) {
    offset := offset + io.in.bits.size

    out_totalBytes := io.in.bits.size
    out_data := io.in.bits.data
    out_addr := baseAddr + offset
  }

  out_valid := io.in.valid

}

class BasebandIO(val addrBits: Int, val beatBytes: Int) extends Bundle {
  val constants = Input(new BasebandConstants)
  val control = new BasebandControlIO(addrBits)
  val dma = new BasebandDMAIO(addrBits, beatBytes)
  val modem = new Bundle {
    val digital = Flipped(new GFSKModemDigitalIO)
    val control = new Bundle {
      val preambleDetected = Input(Bool())
    }
  }
}

class Baseband(params: BLEBasebandModemParams, beatBytes: Int) extends Module {
  val io = IO(new BasebandIO(params.paddrBits, beatBytes))

  val dmaPacketDisassembler = Module(new DMAPacketDisassembler(beatBytes))
  val assembler = Module(new PacketAssembler)

  dmaPacketDisassembler.io.consumer.done := assembler.io.out.control.done
  dmaPacketDisassembler.io.dmaIn <> io.dma.readData

  assembler.io.constants := io.constants
  assembler.io.in.data <> dmaPacketDisassembler.io.consumer.data
  assembler.io.in.control <> io.control.assembler.in

  io.control.assembler.out <> assembler.io.out.control


  val dmaPacketAssembler = Module(new DMAPacketAssembler(beatBytes))
  val disassembler = Module(new PacketDisassembler)
  dmaPacketAssembler.io.producer.done := disassembler.io.out.control.done
  dmaPacketAssembler.io.producer.data <> disassembler.io.out.data

  disassembler.io.constants := io.constants
  disassembler.io.in.control <> io.control.disassembler.in
  disassembler.io.in.preambleDetected := io.modem.control.preambleDetected

  io.control.disassembler.out <> disassembler.io.out.control


  val dmaAddresser = Module(new BasebandDMAAddresser(params.paddrBits, beatBytes))
  dmaAddresser.io.in <> dmaPacketAssembler.io.dmaOut
  dmaAddresser.io.baseAddr <> io.control.baseAddr
  io.dma.writeReq <> dmaAddresser.io.out

  val postAssemblerLoopback = Module(new DecoupledLoopback(UInt(1.W)))
  postAssemblerLoopback.io.select := io.control.loopback(1)
  postAssemblerLoopback.io.left.in <> assembler.io.out.data
  io.modem.digital.tx <> postAssemblerLoopback.io.right.out
  postAssemblerLoopback.io.right.in <> io.modem.digital.rx
  disassembler.io.in.data <> postAssemblerLoopback.io.left.out
}