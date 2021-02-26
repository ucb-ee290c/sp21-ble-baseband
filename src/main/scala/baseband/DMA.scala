package baseband

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.subsystem.{CacheBlockBytes, SystemBusKey}
import freechips.rocketchip.tile.{CoreBundle, HasCoreParameters, XLen}
import freechips.rocketchip.tilelink.{TLIdentityNode, TLXbar}
import testchipip.TLHelper

class BasebandWriterReq(addrBits: Int, beatBytes: Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val data = UInt((beatBytes * 8).W)
  val totalBytes = UInt(log2Ceil(beatBytes).W)
}

class BasebandReaderReq(addrBits: Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val totalBytes = UInt(9.W)
}

class BasebandReaderResp extends Bundle {
  val bytesRead = UInt(9.W)
}

class BasebandDMAWriteIO(addrBits: Int, beatBytes: Int)(implicit p: Parameters) extends CoreBundle {
  val req = Decoupled(new BasebandWriterReq(addrBits, beatBytes))
}

class BasebandDMAReadIO(addrBits: Int, beatBytes: Int)(implicit p: Parameters) extends CoreBundle {
  val req = Flipped(Decoupled(new BasebandReaderReq(addrBits)))
  val resp = Decoupled(new BasebandReaderResp)
  val queue = Decoupled(UInt((beatBytes * 8).W))
}


class BasebandDMA(implicit p: Parameters) extends LazyModule {
  val id_node = TLIdentityNode()
  val xbar_node = TLXbar()

  val beatBytes = p(SystemBusKey).beatBytes

  val reader = LazyModule(new BasebandReader(beatBytes))
  val writer = LazyModule(new BasebandWriter(beatBytes))

  xbar_node := writer.node
  xbar_node := reader.node
  id_node := xbar_node

  lazy val module = new LazyModuleImp(this) with HasCoreParameters {
    val io = IO(new Bundle {
      val read = new BasebandDMAReadIO(reader.module.addrBits, beatBytes)
      val write = new BasebandDMAWriteIO(writer.module.addrBits, beatBytes)
      val busy = Output(Bool())
    })

    val readQ = Queue(reader.module.io.queue) // Queue of read data
    val writeQ = Queue(io.write.req) // Queue of write requests

    io.read.queue <> readQ
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

    val addrBits = edge.bundle.addressBits

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new BasebandWriterReq(addrBits, beatBytes)))
      val busy = Bool()
    })

    val req = Reg(new BasebandWriterReq(addrBits, beatBytes))

    val s_idle :: s_queue :: s_write :: s_resp :: s_done :: Nil = Enum(4)
    val state = RegInit(s_idle)

    val bytesSent = Reg(UInt(log2Ceil(beatBytes).W))
    val bytesLeft = req.totalBytes - bytesSent

    val put = edge.Put(
      fromSource = 0.U, // TODO: verify
      toAddress = req.addr,
      lgSize = log2Ceil(beatBytes).U,
      data = req.data)._2 // TODO: comes from queue

    val putPartial = edge.Put(
      fromSource = 0.U,
      toAddress = req.addr,
      lgSize = log2Ceil(beatBytes).U,
      data = req.data,
      mask = 0.U)._2

    mem.a.valid := state === s_write
    mem.a.bits := Mux(bytesLeft < beatBytes.U, put, putPartial)

    mem.d.ready := state === s_resp

    when (edge.done(mem.a)) {
      req.addr := req.addr + beatBytes.U
      bytesSent := bytesSent + beatBytes.U // TODO: update for put partial
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
    val addrBits = edge.bundle.addressBits

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new BasebandReaderReq(addrBits)))
      val resp = Decoupled(new BasebandReaderResp)
      val queue = Decoupled(UInt((beatBytes * 8).W))
      val busy = Bool()
    })

    val req = Reg(new BasebandReaderReq(addrBits))

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
      bytesRead := bytesRead + Mux(bytesLeft < beatBytes.U, bytesLeft, beatBytes.U)
      state := s_resp
    }

    when (mem.d.fire()) {
      io.queue.bits := mem.d.bits.data
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
