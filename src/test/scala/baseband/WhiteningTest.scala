package baseband

import breeze.plot._
import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import modem.{GFSKModemAnalogIO, GFSKModemLUTCodes, GFSKModemLUTCommand, TestUtility}
import org.scalatest.flatspec.AnyFlatSpec

import verif._
import  ee290cdma._
import scala.collection.immutable.Seq
import scala.util.Random

class WhiteningTestModule extends Module {
  val io = IO(new Bundle {
    val operand  = Flipped(Valid(UInt(1.W)))
    val result   = Valid(UInt(1.W))
    val channelIndex     = Input(UInt(6.W))
    val init     = Input(Bool()) // TODO: Why can't this seed, init combo be replaced with a valid interface on seed?
  })

  val whitening = Module(new Whitening)
  whitening.io.operand <> io.operand
  whitening.io.init := io.init
  io.result <> whitening.io.result
  whitening.io.seed := Cat(Reverse(io.channelIndex), 1.U(1.W))
}


class WhiteningTest extends AnyFlatSpec with ChiselScalatestTester {
  val params = BLEBasebandModemParams()
  val incorrectbleSpecCI0WhiteningBits = Seq(0,0,0,0,0,0,1,0, 0,1,0,0,1,1,0,1, 0,0,1,1,1,1,0,1, 1,1,0,0,0,1,0,1, 1,1,1,1,1,0,0,0, 1,1,1,0,1,1,0,0, 0,1,0,1,0,0,1,0, 1,1,1,1,1,0,1,0)
  val bleSpecCI0WhiteningBits = Seq(0,0,0,0,0,0,1,0, 0,1,0,0,1,1,0,1, 0,0,1,1,1,1,0,1, 1,1,0,0,0,0,1,1, 1,1,1,1,1,0,0,0, 1,1,1,0,1,1,0,0, 0,1,0,1,0,0,1,0, 1,1,1,1,1,0,1,0)
  val bleSpecCI9WhiteningBits = Seq(1,0,0,0,0,0,1,1, 0,1,1,0,1,0,1,1, 1,0,1,0,0,0,1,1, 0,0,1,0,0,0,1,0, 0,0,0,0,0,1,0,0, 1,0,0,1,1,0,1,0, 0,1,1,1,1,0,1,1, 1,0,0,0,0,1,1,1)
  val bleSpecCI22WhiteningBits = Seq(0,1,1,0,0,1,1,0, 0,0,0,0,1,1,0,1, 1,0,1,0,1,1,1,0, 1,0,0,0,1,1,0,0, 1,0,0,0,1,0,0,0, 0,0,0,1,0,0,1,0, 0,1,1,0,1,0,0,1, 1,1,1,0,1,1,1,0)
  val bleSpecCI38WhiteningBits = Seq(0,1,1,0,1,0,1,1, 1,0,1,0,0,0,1,1, 0,0,1,0,0,0,1,0, 0,0,0,0,0,1,0,0, 1,0,0,1,1,0,1,0, 0,1,1,1,1,0,1,1, 1,0,0,0,0,1,1,1, 1,1,1,1,0,0,0,1)

  val bleSpecCI38Prewhitened = Seq(0,1,0,0,0,0,1,0, 1,0,0,1,0,0,0,0, 0,1,1,0,0,1,0,1, 1,0,1,0,0,1,0,1, 0,0,1,0,0,1,0,1, 1,1,0,0,0,1,0,1, 0,1,0,0,0,1,0,1,
    1,0,0,0,0,0,1,1, 1,0,0,0,0,0,0,0, 0,1,0,0,0,0,0,0, 1,1,0,0,0,0,0,0, 1,0,1,1,0,1,0,1, 0,0,1,0,1,1,0,1, 1,1,0,1,0,1,1,1)
  val bleSpecCI38Whitened = Seq(0,0,1,0,1,0,0,1, 0,0,1,1,0,0,1,1, 0,1,0,0,0,1,1,1, 1,0,1,0,0,0,0,1, 1,0,1,1,1,1,1,1, 1,0,1,1,1,1,1,0, 1,1,0,0,0,0,1,0,
    0,1,1,1,0,0,1,0, 0,1,0,1,1,0,0,0, 1,1,1,0,0,1,0,1, 0,0,1,1,0,1,0,1, 1,1,1,1,0,1,1,1, 1,1,1,1,0,0,1,1, 1,0,1,0,0,1,0,1)


  it should "Verify Software Model" in {
    test(new Whitening()).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      assert(TestUtility.getWhiteningBits(64, 0) != incorrectbleSpecCI0WhiteningBits)
      assert(TestUtility.getWhiteningBits(64, 0) == bleSpecCI0WhiteningBits)
      assert(TestUtility.getWhiteningBits(64, 9) == bleSpecCI9WhiteningBits)
      assert(TestUtility.getWhiteningBits(64, 22) == bleSpecCI22WhiteningBits)
      assert(TestUtility.getWhiteningBits(64, 38) == bleSpecCI38WhiteningBits)
      assert(TestUtility.whiten(bleSpecCI38Prewhitened, 38) == bleSpecCI38Whitened)
    }
  }

  it should "whiten bits correctly" in {
    test(new WhiteningTestModule()).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      for (channelIndex <- 0 until 40) {
        val input = Seq.tabulate(1000){_ => Random.nextInt(2)}
        var result = Seq[Int]()
        c.io.channelIndex.poke(channelIndex.U)
        c.io.init.poke(1.B)
        c.clock.step()
        c.io.init.poke(0.B)
        for (i <- input) {
          c.io.operand.valid.poke(1.B)
          c.io.operand.bits.poke(i.U)
          result = result ++ Seq(c.io.result.bits.peek().litValue().toInt)
          c.clock.step()
        }
        assert(result == TestUtility.whiten(input, channelIndex))
      }
    }
  }

}
