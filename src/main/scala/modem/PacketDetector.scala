package modem

import chisel3._
import chisel3.util._

class PacketDetector extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(1.W))
    val firstBit = Input(UInt(1.W))
    val matches = Output(UInt(log2Ceil(20 * 8 + 1).W))
  })

  val cells = Seq.tabulate(8){ i => Seq.tabulate(20){_ => i} }.reverse.flatten.map {i => (i % 2 == 0, Module(new CorrelatorCell).io)}
  cells.head._2.in := io.in
  if (cells.head._1) {
    cells.head._2.expected := io.firstBit
  } else {
    cells.head._2.expected := !io.firstBit
  }
  cells.head._2.sumSoFar := 0.U

  for ((current, next) <- cells.zip(cells.tail)) {
    next._2.in := current._2.out
    if (next._1) {
      next._2.expected := io.firstBit
    } else {
      next._2.expected := !io.firstBit
    }
    next._2.sumSoFar := current._2.sum
  }
  io.matches := cells.last._2.sum
}

class CorrelatorCell extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(1.W))
    val sumSoFar = Input(UInt(8.W))
    val expected = Input(UInt(1.W))
    val sum = Output(UInt(8.W))
    val out = Output(UInt(1.W))
  })
  io.out := RegNext(io.in)
  io.sum := io.sumSoFar + (io.in === io.expected)
}