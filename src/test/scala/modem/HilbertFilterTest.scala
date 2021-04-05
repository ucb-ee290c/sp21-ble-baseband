package modem

import baseband.BLEBasebandModemParams
import modem.HilbertFilter
import breeze.linalg.DenseVector
import chisel3.UInt
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.ChiselScalatestTester
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec
import breeze.signal.{designFilterFirwin, filterLP}
import breeze.signal.support.CanFilterLPHP

import scala.math

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
      c.io.out.data.ready.poke(1.B)
      var arr = Seq[BigInt]()
      var i = 0
      while (i < mock_input_I_image.length) {
        c.io.in.i.bits.poke(mock_input_I_image(i).asUInt())
        c.io.in.q.bits.poke(mock_input_Q_image(i).asUInt())
        c.io.in.q.valid.poke((i < mock_input_I_rf.length).asBool())
        c.io.in.i.valid.poke((i < mock_input_I_rf.length).asBool())
        c.clock.step()
       if (c.io.out.data.valid.peek().litToBoolean)
          arr = arr ++ Seq(c.io.out.data.bits.peek().litValue())
        i+=1
      }
      assert(arr.max - arr.min < mock_input_I_image.max - mock_input_I_image.min)
    }
  }

  it should "Do something else" in {
    test(new HilbertFilter(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.out.data.ready.poke(1.B)
      var arr = Seq[BigInt]()
      var i = 0
      while (i < mock_input_I_mixed.length) {
        c.io.in.i.bits.poke(mock_input_I_mixed(i).asUInt())
        c.io.in.q.bits.poke(mock_input_Q_mixed(i).asUInt())
        c.io.in.q.valid.poke((i < mock_input_I_mixed.length).asBool())
        c.io.in.i.valid.poke((i < mock_input_I_mixed.length).asBool())
        c.clock.step()
       if (c.io.out.data.valid.peek().litToBoolean)
          arr = arr ++ Seq(c.io.out.data.bits.peek().litValue())
        i+=1
      }
      print("MIXED:\n")
      print(arr)
      assert(arr.max - arr.min > mock_input_I_mixed.max - mock_input_I_mixed.min)
    }
  }

  it should "Do something else again" in {
    test(new HilbertFilter(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.out.data.ready.poke(1.B)
      var arr = Seq[BigInt]()
      var i = 0
      while (i < mock_input_I_rf.length) {
        c.io.in.i.bits.poke(mock_input_I_rf(i).asUInt())
        c.io.in.q.bits.poke(mock_input_Q_rf(i).asUInt())
        c.io.in.q.valid.poke((i < mock_input_I_rf.length).asBool())
        c.io.in.i.valid.poke((i < mock_input_I_rf.length).asBool())
        c.clock.step()
       if (c.io.out.data.valid.peek().litToBoolean)
          arr = arr ++ Seq(c.io.out.data.bits.peek().litValue())
        i+=1
      }
      print("RF ONLY:\n")
      print(arr)
      assert(arr.max - arr.min > mock_input_I_rf.max - mock_input_I_rf.min)
    }
  }

}
