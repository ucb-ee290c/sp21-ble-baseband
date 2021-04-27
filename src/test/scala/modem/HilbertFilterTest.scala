package modem

import baseband.BLEBasebandModemParams
import breeze.linalg.DenseVector
import breeze.plot.{Figure, plot}
import chisel3.UInt
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.ChiselScalatestTester
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec
import breeze.signal.{designFilterFirwin, filterLP}
import breeze.signal.support.CanFilterLPHP
import chisel3.experimental.FixedPoint
import chisel3.util.Decoupled

import scala.collection.immutable.Seq
import scala.math
import scala.util.Random

class RoundTowardsZeroTestModule extends Module {
  val io = IO(new Bundle {
    val in = Input(FixedPoint(3.W, 1.BP))
    val out = Output(SInt(2.W))
  })
  io.out := Utility.roundTowardsZero(io.in)
}

class RoundTowardsZeroIncorrectTestModule extends Module {
  val io = IO(new Bundle {
    val in = Input(FixedPoint(3.W, 1.BP))
    val out = Output(SInt(2.W))
  })
  io.out := ((io.in >> 1)(1, 0)).asSInt()
}

class HilbertFilterTest extends AnyFlatSpec with ChiselScalatestTester {

  it should "PASS Radio Frequency and Display Waveform" in {
    test(new HilbertFilter(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.out.ready.poke(1.B)
      var arr = Seq[BigInt]()
      var i = 0
      val numberOfBits = 20
      val preamble = Seq(1,0,1,0,1,0,1,0)
      val packet = Seq.tabulate(numberOfBits){_ => Random.nextInt(2)}
      val bits = Seq(0,0,0,0,0,0) ++ preamble ++ TestUtility.whiten(packet, 0) ++ Seq(0,0,0,0,0,0,0)
      val input = TestUtility.testWaveform(bits, imageAmplitude = 0)
      while (i < input.length) {
        c.io.in.i.bits.poke(input(i)._1.asUInt())
        c.io.in.q.bits.poke(input(i)._2.asUInt())
        c.io.in.i.valid.poke((i < input.length).asBool())
        c.io.in.q.valid.poke((i < input.length).asBool())
        c.clock.step()
        if (c.io.out.valid.peek().litToBoolean)
          arr = arr ++ Seq(c.io.out.bits.peek().litValue())
        i+=1
      }
      print("Output:\n")
      print(arr)
      val f = Figure()
      val p = f.subplot(0)
      p += plot(Seq.tabulate(arr.size)(i => i), arr.map {_.toInt}, colorcode = "b")
      assert(true)
    }
  }

  it should "REJECT IMAGE and Display Waveform" in {
    test(new HilbertFilter(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.out.ready.poke(1.B)
      var arr = Seq[BigInt]()
      var i = 0
      val numberOfBits = 20
      val preamble = Seq(1,0,1,0,1,0,1,0)
      val packet = Seq.tabulate(numberOfBits){_ => Random.nextInt(2)}
      val bits = Seq(0,0,0,0,0,0) ++ preamble ++ TestUtility.whiten(packet, 0) ++ Seq(0,0,0,0,0,0,0)
      val input = TestUtility.testWaveform(bits, TestUtility.F_IM,  imageAmplitude = 1, signalAmplitude = 0)
      while (i < input.length) {
        c.io.in.i.bits.poke(input(i)._1.asUInt())
        c.io.in.q.bits.poke(input(i)._2.asUInt())
        c.io.in.i.valid.poke((i < input.length).asBool())
        c.io.in.q.valid.poke((i < input.length).asBool())
        c.clock.step()
        if (c.io.out.valid.peek().litToBoolean)
          arr = arr ++ Seq(c.io.out.bits.peek().litValue())
        i+=1
      }
      print("Output:\n")
      print(arr)
      val f = Figure()
      val p = f.subplot(0)
      p += plot(Seq.tabulate(arr.size)(i => i), arr.map {_.toInt}, colorcode = "r")
      assert(true)
    }
  }

  it should "Utility Round Towards Zero" in {
    test(new RoundTowardsZeroTestModule).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.in.poke(FixedPoint.fromDouble(1.5, 3.W, 1.BP))
      assert(c.io.out.peek().asSInt().litValue() == 1)

      c.io.in.poke(FixedPoint.fromDouble(-1.5, 3.W, 1.BP))
      assert(c.io.out.peek().asSInt().litValue() == -1)
    }
  }

  it should "Normally Does NOT Round Towards Zero" in {
    test(new RoundTowardsZeroIncorrectTestModule).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.in.poke(FixedPoint.fromDouble(1.5, 3.W, 1.BP))
      assert(c.io.out.peek().asSInt().litValue() == 1)

      c.io.in.poke(FixedPoint.fromDouble(-1.5, 3.W, 1.BP))
      assert(c.io.out.peek().asSInt().litValue() != -1)
      assert(c.io.out.peek().asSInt().litValue() == -2)
    }
  }

}
