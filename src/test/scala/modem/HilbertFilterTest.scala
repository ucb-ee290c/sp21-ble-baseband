package modem

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
  val time_interval = 0.00001
  val digital_clock_F = 20 * MHz


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

  val t = timeSequence(time_interval, analog_F_sample)
  val RF = cosSignal(F_RF)
  val IM = cosSignal(F_IM)
  var I = lowpass(5, analog_F_sample, 3.5 * MHz)( DenseVector(t.map(t => RF(t) * cosSignal(F_LO)(t)).toArray))
  var Q = lowpass(5, analog_F_sample, 3.5 * MHz)( DenseVector(t.map(t => RF(t) * cosSignal(F_LO)(t)).toArray))
  var sampled_I = I.toArray.zipWithIndex.collect {case (e, i) if i % (analog_F_sample / digital_clock_F).floor == 0 => e}
  var sampled_Q = Q.toArray.zipWithIndex.collect {case (e, i) if i % (analog_F_sample / digital_clock_F).floor == 0 => e}


  it should "Do something" in {
    test(new HilbertFilter()).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      print(sampled_I)
      print(sampled_Q)
    }
  }
}
