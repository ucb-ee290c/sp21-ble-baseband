package modem

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec
import baseband.BLEBasebandModemParams
import chisel3.util.{Counter, ShiftRegister}
import scala.util.Random
import scala.collection.immutable.Seq

class PacketDetectorTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "Perfectly Match Packet Preamble with leading 0" in {
    test(new PreambleCorrelator()).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.firstBit.poke(0.U)
      c.io.in.poke(0.U)
      Seq.tabulate(8){i => i}.flatMap{ i => Seq.tabulate(20){ _ => i%2 } }.foreach { e =>
        c.clock.step()
        c.io.in.poke(e.asUInt())
      }
      assert(c.io.matches.peek().litValue() == 20 * 8)
    }
  }
  it should "Perfectly Match Packet Preamble with leading 1" in {
    test(new PreambleCorrelator()).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.firstBit.poke(1.U)
      c.io.in.poke(0.U)
      Seq.tabulate(8){i => i}.flatMap{ i => Seq.tabulate(20){ _ => if (i%2 == 0) 1 else 0 } }.foreach { e =>
        c.clock.step()
        c.io.in.poke(e.asUInt())
      }
      assert(c.io.matches.peek().litValue() == 20 * 8)
    }
  }
  val packetPattern = Seq.tabulate(8){i => i}.flatMap{ i => Seq.tabulate(20){ _ => i%2 } }
  it should "Pass Fuzz Test" in {
    for (i <- 1 to 100) {
      val seq = Seq.tabulate(8){i => i}.flatMap{ i => Seq.tabulate(20){ _ => Random.nextInt(1) } }
      test(new PreambleCorrelator()).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
        c.io.firstBit.poke(0.U)
        c.io.in.poke(0.U)
        seq.foreach { e =>
          c.clock.step()
          c.io.in.poke(e.asUInt())
        }
        assert(c.io.matches.peek().litValue() == seq.zip(packetPattern).map {e => if (e._1 == e._2) 1 else 0}.sum)
      }
    }
  }

}
