package modem

import baseband.{DecoupledLoopback, Loopback}
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, TreadleBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec


class DelayChainTest extends AnyFlatSpec with ChiselScalatestTester {
    val bitwidth = 8
    it should "Fuzz Delay Chain" in {
      for (i <- 1 to 32) {
        test(new GenericDelayChain(i, UInt(bitwidth.W))).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
          val in = scala.util.Random.nextInt(scala.math.pow(2, bitwidth).toInt)
          c.io.in.bits.poke(in.U)
          c.io.out.ready.poke(1.B)
          c.io.in.valid.poke(1.B)
          for (_ <- 0 until i) { // i time steps
            assert(c.io.in.ready.peek().litToBoolean)
            assert(!c.io.out.valid.peek().litToBoolean)
            c.clock.step()
          }
          assert(c.io.out.valid.peek().litToBoolean)
          print(c.io.out.bits.peek().litValue())
          assert(c.io.out.bits.peek().litValue() == in)
        }
      }
    }
}
