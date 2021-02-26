package baseband

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.diplomacy.{IdRange, LazyModule, LazyModuleImp}
import freechips.rocketchip.rocket.constants.MemoryOpConstants
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tile.{CoreBundle, HasCoreParameters}
import freechips.rocketchip.tilelink.{TLIdentityNode, TLXbar}
import testchipip.TLHelper

class BasebandDMAReadReq extends Bundle {
  val memAddr = UInt(32.W) // 32-bit address space
  val localAddr = UInt() // TODO: Determine internal capacity
  val bytes = UInt(9.W) // 2-258 bytes
}

class BasebandDMAReadResp extends Bundle {
  val bytesRead = UInt(9.W) // 2-258 bytes
}

class BasebandDMAWriteReq extends Bundle {
  val addr = UInt(32.W) // 32-bit address space
}

class BasebandDMAWriteResp extends Bundle {
  val success = Bool()
}

class BasebandDMAReadIO()(implicit p: Parameters) extends CoreBundle {
  val req = Decoupled(new BasebandDMAReadReq)
  val resp = Flipped(Decoupled())
}

class BasebandDMAWriteIO(implicit p: Parameters) extends CoreBundle {
  val req = Decoupled()
  val resp = Flipped(Decoupled())
}

class BasebandDMA(implicit p: Parameters) extends LazyModule {
  val id_node = TLIdentityNode()
  val xbar_node = TLXbar()

  val blockBytes = p(CacheBlockBytes)

  val reader = LazyModule(new BasebandReader(blockBytes))
  val writer = LazyModule(new BasebandWriter(blockBytes))

  xbar_node := writer.node
  xbar_node := reader.node
  id_node := xbar_node



  lazy val module = new LazyModuleImp(this) with HasCoreParameters {
    val io = IO(new Bundle {
      val dma = new Bundle {
        val read = Flipped(new BasebandDMAReadIO)
        val write = new BasebandDMAWriteIO
      }
      val busy = Output(Bool())
    })
  }

}

class BasebandWriterReq(addrBits: Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val totalBytes = UInt(9.W)
}

class BasebandWriterResp extends Bundle {
  val success = Bool()
}

class BasebandWriter(blockBytes: Int)(implicit p: Parameters) extends LazyModule {
  val node = TLHelper.makeClientNode(
    name = "baseband-writer",
    sourceId = IdRange(0, 1)
  )

  lazy val module = new LazyModuleImp(this) with HasCoreParameters with MemoryOpConstants {
    val (mem, edge) = node.out(0)
    val addrBits = edge.bundle.addressBits

    val io = IO(new Bundle {
      val req = Flipped(Decoupled(new BasebandWriterReq(addrBits)))
      val resp = Decoupled(new BasebandWriterResp)
    })

    val req = Reg(new BasebandWriterReq(addrBits))

    val s_idle :: s_write :: s_resp :: s_done :: Nil = Enum(4)
    val state = RegInit(s_idle)

    val bytesSent = Reg(UInt(9.W))
    val bytesLeft = req.totalBytes - bytesSent

    val put = edge.Put(
      fromSource = 0.U, // TODO: What is our source ID? Set Data
      toAddress = req.addr,
      lgSize = log2Ceil(blockBytes).U,
      data = 0.U)._2

    val putPartial = edge.Put(
      fromSource = 0.U, // TODO: What is our source ID? Set Data and Mask
      toAddress = req.addr,
      lgSize = log2Ceil(blockBytes).U,
      data = 0.U,
      mask = 0.U)._2

    mem.a.valid := state === s_write
    mem.a.bits := Mux(bytesLeft < blockBytes.U, put, putPartial)

    mem.d.ready := state === s_resp

    when (edge.done(mem.a)) {
      req.addr := req.addr + blockBytes.U
      bytesSent := bytesSent + blockBytes.U
      state := s_resp
    }

    when (mem.d.fire()) {
      state := Mux(bytesLeft === 0.U, s_done, s_write)
    }

    io.req.ready := state === s_idle | state === s_done

    io.resp.valid := state === s_done
    io.resp.bits.success := state === s_done // TODO: Better way to track success based on d contents?

    when (io.req.fire()) {
      req := io.req.bits
      bytesSent := 0.U
      state := s_write
    }
  }
}

class BasebandReaderReq(addrBits: Int) extends Bundle {
  val addr = UInt(addrBits.W)
  val totalBytes = UInt(9.W)
}

class BasebandReaderResp extends Bundle {
  val success = Bool()
}

class BasebandReader(blockBytes: Int)(implicit p: Parameters) extends LazyModule {
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
    })

    val req = Reg(new BasebandReaderReq(addrBits))

    val get = edge.Get(
      fromSource = 0.U, // TODO: see writer source comment
      toAddress = req.addr,
      lgSize = 0.U)._2 // TODO: get size

    mem.a.bits := get

    when (io.req.fire()) {
      req := io.req.bits
    }
  }
}
