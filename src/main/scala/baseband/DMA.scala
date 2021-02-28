package baseband

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.subsystem.{SystemBusKey}
import freechips.rocketchip.tile.{CoreBundle, HasCoreParameters}
import freechips.rocketchip.tilelink.{TLIdentityNode, TLXbar}
import testchipip.TLHelper

class BasebandWriterReq(addrBits: Int, beatBytes: Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val data = UInt((beatBytes * 8).W)
  val totalBytes = UInt(log2Ceil(beatBytes).W)
}

class BasebandReaderReq(addrBits: Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val totalBytes = UInt(9.W) // TODO: Make this not a magic number and instead base on some maxReadSize = 258 bytes
}

class BasebandReaderResp extends Bundle {
  val bytesRead = UInt(9.W) // TODO: See previous magic number comment
}

class BasebandDMAWriteIO(addrBits: Int, beatBytes: Int) extends Bundle {
  val req = Decoupled(new BasebandWriterReq(addrBits, beatBytes))
}

class BasebandDMAReadIO(addrBits: Int, beatBytes: Int) extends Bundle {
  val req = Flipped(Decoupled(new BasebandReaderReq(addrBits)))
  val resp = Decoupled(new BasebandReaderResp)
  val queue = Decoupled(UInt((beatBytes * 8).W))
}


class BasebandDMA(beatBytes: Int)(implicit p: Parameters) extends LazyModule {
  val id_node = TLIdentityNode()
  val xbar_node = TLXbar()

  val reader = LazyModule(new BasebandReader(beatBytes))
  val writer = LazyModule(new BasebandWriter(beatBytes))

  xbar_node := writer.node
  xbar_node := reader.node
  id_node := xbar_node

  lazy val module = new LazyModuleImp(this) with HasCoreParameters {
    val io = IO(new Bundle {
      val read = new BasebandDMAReadIO(paddrBits, beatBytes)
      val write = new BasebandDMAWriteIO(paddrBits, beatBytes)
      val busy = Output(Bool())
    })

    val readQ = Queue(reader.module.io.queue) // Queue of read data
    val writeQ = Queue(io.write.req) // Queue of write requests

    io.read.queue <> readQ

    reader.module.io.req <> io.read.req
    reader.module.io.resp <> io.read.resp

    writer.module.io.req <> writeQ

    io.busy := writer.module.io.busy | reader.module.io.busy
  }

}

class BasebandWriter(beatBytes: Int)(implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "baseband-writer",
    sourceId = IdRange(0, 1)
  )

  lazy val module = new LazyModuleImp(this) with HasCoreParameters with MemoryOpConstants {
    val (mem, edge) = node.out(0)

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new BasebandWriterReq(paddrBits, beatBytes)))
      val busy = Bool()
    })

    val req = Reg(new BasebandWriterReq(paddrBits, beatBytes))

    val s_idle :: s_write :: s_resp :: s_done :: Nil = Enum(4)
    val state = RegInit(s_idle)

    val mask = VecInit(Seq.tabulate(beatBytes)(i => ((1 << i) - 1).U ))

    val bytesSent = Reg(UInt(log2Ceil(beatBytes).W))
    val bytesLeft = req.totalBytes - bytesSent

    val put = edge.Put(
      fromSource = 0.U, // TODO: Verify
      toAddress = req.addr,
      lgSize = log2Ceil(beatBytes).U,
      data = req.data)._2

    val putPartial = edge.Put(
      fromSource = 0.U,
      toAddress = req.addr,
      lgSize = log2Ceil(beatBytes).U,
      data = req.data,
      mask = mask(bytesLeft))._2

    mem.a.valid := state === s_write
    mem.a.bits := Mux(bytesLeft < beatBytes.U, put, putPartial)

    mem.d.ready := state === s_resp

    when (edge.done(mem.a)) {
      req.addr := req.addr + beatBytes.U
      bytesSent := bytesSent + Mux(bytesLeft < beatBytes.U, bytesLeft, beatBytes.U)
      state := s_resp
    }

    when (mem.d.fire()) {
      state := Mux(bytesLeft === 0.U, s_done, s_write)
    }

    io.req.ready := state === s_idle | state === s_done
    io.busy := ~io.req.ready

    when (io.req.fire()) {
      req := io.req.bits
      bytesSent := 0.U
      state := s_write
    }
  }
}

class BasebandReader(beatBytes: Int)(implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "baseband-reader",
    sourceId = IdRange(0, 1)
  )

  lazy val module = new LazyModuleImp(this) with HasCoreParameters with MemoryOpConstants {
    val (mem, edge) = node.out(0)

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new BasebandReaderReq(paddrBits)))
      val resp = Decoupled(new BasebandReaderResp)
      val queue = Decoupled(UInt((beatBytes * 8).W))
      val busy = Bool()
    })

    val req = Reg(new BasebandReaderReq(paddrBits))

    val s_idle :: s_read :: s_resp :: s_queue :: s_done :: Nil = Enum(5)
    val state = RegInit(s_idle)

    val bytesRead = Reg(UInt(9.W))
    val bytesLeft = req.totalBytes - bytesRead


    mem.a.bits := edge.Get(
      fromSource = 0.U, // TODO: see writer source comment
      toAddress = req.addr,
      lgSize = log2Ceil(beatBytes).U)._2 // Always get a full beatBytes bytes, even if not used in packet

    when (edge.done(mem.a)) {
      req.addr := req.addr + beatBytes.U
      bytesRead := bytesRead + Mux(bytesLeft < beatBytes.U, bytesLeft, beatBytes.U) // TODO: move down to mem.d.fire clause to allow for masking (?)
      state := s_resp
    }

    when (mem.d.fire()) {
      io.queue.bits := mem.d.bits.data // TODO: mask off the unwanted bytes if bytesLeft < beatBytes.U using a mask vector and register
      state := s_queue
    }

    when (io.queue.fire()) {
      state := Mux(bytesLeft === 0.U, s_done, s_read)
    }

    io.req.ready := state === s_idle | state === s_done
    io.resp.valid := state === s_done
    io.resp.bits.bytesRead := bytesRead
    io.queue.valid := state === s_queue
    io.busy := ~io.req.ready

    when (io.req.fire()) {
      req := io.req.bits
      state := s_read
    }
  }
}
