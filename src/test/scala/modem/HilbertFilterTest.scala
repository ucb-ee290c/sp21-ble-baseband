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

  val MHz = 1000000

  val channel_index = 0
  val F_RF = (2402 + 2 * channel_index) * MHz
  val F_IF = 2.5 * MHz
  val F_LO = F_RF - F_IF
  val F_IM = F_LO - F_IF
  val analog_F_sample = (F_LO * 2 + F_IF) * 2
  val time_interval = 0.0000001
  val digital_clock_F = 20 * MHz

  val mock_input_I_mixed = Array(15, 15, 15, 15, 16, 17, 19, 20, 17, 11, 4, 3, 10, 22, 30, 29, 19, 7, 0, 2, 12, 24, 30, 27, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28)
  val mock_input_Q_mixed = Array(15, 15, 15, 15, 14, 13, 11, 10, 13, 19, 26, 27, 20, 8, 0, 1, 11, 23, 30, 28, 18, 6, 0, 3, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2)
  val mock_input_I_image = Array(15, 15, 15, 15, 15, 17, 20, 24, 24, 19, 9, 2, 2, 10, 22, 30, 28, 18, 6, 0, 2, 12, 23, 30, 27, 18, 7, 0, 3, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 30, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 28, 18, 7, 0, 2, 12, 23, 29, 28, 18, 7, 0, 2, 12, 23, 29, 28, 18, 7, 0, 2, 12, 23, 29, 28, 18, 7, 0, 2, 12, 23, 29, 28, 18, 7, 0, 2, 12, 23, 29, 28, 18, 7, 0, 2, 12, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 19, 7, 0, 2, 11, 23, 29, 28, 19, 7, 0, 2, 11, 23, 29)
  val mock_input_Q_image = Array(15, 15, 15, 15, 15, 14, 14, 16, 21, 25, 25, 19, 9, 1, 2, 11, 23, 30, 28, 18, 6, 0, 2, 12, 23, 30, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 27, 18, 7, 0, 2, 12, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11, 23, 29, 28, 18, 7, 0, 2, 11)
  var mock_input_I_rf = Array(15, 15, 15, 15, 15, 16, 16, 14, 9, 5, 5, 11, 21, 29, 28, 19, 7, 0, 2, 12, 24, 30, 28, 18, 7, 0, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19)
  var mock_input_Q_rf = Array(15, 15, 15, 15, 15, 13, 10, 6, 6, 11, 21, 28, 28, 20, 8, 0, 2, 12, 24, 30, 28, 18, 7, 0, 3, 12, 23, 30, 27, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 0, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 3, 12, 23, 30, 28, 18, 7, 1, 2, 12, 23, 30, 28, 18, 7, 1, 2, 12, 23, 30, 28, 18, 7, 1, 2, 12, 23, 30, 28, 18, 7, 1, 2, 12, 23, 30, 28, 18, 7, 1, 2, 12, 23, 30, 28, 18, 7, 1, 2, 12, 23, 30, 28, 18, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 12, 23, 30, 28, 19, 7, 1, 2, 11, 23, 30, 28, 19, 7, 1, 2, 11, 23, 30, 28, 19, 7, 1)


  def lowpass(order: Int, sampleFrequency: Double, cutoffFrequency: Double): DenseVector[Double] => DenseVector[Double] = {
    return (x: DenseVector[Double]) => {filterLP(data = x, omega = cutoffFrequency, sampleRate = sampleFrequency )(CanFilterLPHP.dvDouble1DFilterLPHP)}
  }

  def timeSequence(duration: Double, frequency: Double): Seq[Double] = {
    val time_steps: Seq[Double] = Seq()
    var currentTime: Double = 0
    while (currentTime <= duration) {
      time_steps ++ Seq(currentTime)
      currentTime += (1/frequency)
    }
    return time_steps
  }

  def cosSignal(frequency: Double, phaseOffset: Double = 0): (Double) => Double = {
    return {(t: Double) => math.cos(2 * math.Pi * frequency * t + phaseOffset)}
  }
  def sinSignal(frequency: Double, phaseOffset: Double = 0): (Double) => Double = {
    return {(t: Double) => math.sin(2 * math.Pi * frequency * t + phaseOffset)}
  }
/*
  val t = timeSequence(time_interval, analog_F_sample)
  val RF = cosSignal(F_RF)
  val IM = cosSignal(F_IM)
  var I = lowpass(5, analog_F_sample, 3.5 * MHz)( DenseVector(t.map(t => RF(t) * cosSignal(F_LO)(t)).toArray))
  var Q = lowpass(5, analog_F_sample, 3.5 * MHz)( DenseVector(t.map(t => RF(t) * cosSignal(F_LO)(t)).toArray))
  var sampled_I = I.toArray.zipWithIndex.collect {case (e, i) if i % (analog_F_sample / digital_clock_F).floor == 0 => e}
  var sampled_Q = Q.toArray.zipWithIndex.collect {case (e, i) if i % (analog_F_sample / digital_clock_F).floor == 0 => e}
*/

  it should "REJECT IMAGE" in {
    test(new HilbertFilter(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.out.ready.poke(1.B)
      var arr = Seq[BigInt]()
      var i = 0
      while (i < mock_input_I_image.length) {
        c.io.in.i.bits.poke(mock_input_I_image(i).asUInt())
        c.io.in.q.bits.poke(mock_input_Q_image(i).asUInt())
        c.io.in.i.valid.poke((i < mock_input_I_rf.length).asBool())
        c.io.in.q.valid.poke((i < mock_input_I_rf.length).asBool())
        c.clock.step()
       if (c.io.out.valid.peek().litToBoolean)
          arr = arr ++ Seq(c.io.out.bits.peek().litValue())
        i+=1
      }
      assert(arr.max - arr.min < mock_input_I_image.max - mock_input_I_image.min)
    }
  }

  it should "PASS Radio Frequency and Display Waveform" in {
    test(new HilbertFilter(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.out.ready.poke(1.B)
      var arr = Seq[BigInt]()
      var i = 0
      val numberOfBits = 20
      val preamble = Seq(1,0,1,0,1,0,1,0)
      val packet = Seq.tabulate(numberOfBits){_ => Random.nextInt(2)}
      val bits = Seq(0,0,0,0,0,0) ++ preamble ++ TestUtility.whiten(packet, 0) ++ Seq(0,0,0,0,0,0,0)
      val input = TestUtility.testWaveform(bits)
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
     /* val f = Figure()
      val p = f.subplot(0)
      p += plot(Seq.tabulate(arr.size)(i => i), arr.map {_.toInt}, colorcode = "b")*/
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
      val input = TestUtility.testWaveform(bits, TestUtility.F_IM)
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
      /*
      val f = Figure()
      val p = f.subplot(0)
      p += plot(Seq.tabulate(arr.size)(i => i), arr.map {_.toInt}, colorcode = "r")*/
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
