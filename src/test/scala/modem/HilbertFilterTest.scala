package modem

import baseband.BLEBasebandModemParams
import chisel3.UInt
import chiseltest.ChiselScalatestTester
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec
import net.sparja.syto.filter.{TransferFunctionBuilder, filterForward}

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

  def butterLowpass(order: Int, sampleFrequency: Double, cutoffFrequency: Double): List[Double] => Seq[Double] = {

    val (b, a) = new TransferFunctionBuilder()
      .butterworthApproximation(order)  // The order of Butterworth filter
      .digitalize(sampleFrequency)  // digital filter with sampling rate at 30 Hz
      .transformToLowPass(cutoffFrequency) // Low-pass filter with cutoff frequency 3.5Hz
      .coefficients
    return (x: List[Double]) => {filterForward(b, a, x)}
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
  var I = butterLowpass(5, analog_F_sample, 3.5 * MHz)(t.map(t => RF(t) * cosSignal(F_LO)(t)).toList)
  var Q = butterLowpass(5, analog_F_sample, 3.5 * MHz)(t.map(t => RF(t) * cosSignal(F_LO)(t)).toList)
  I = I.zipWithIndex.collect {case (e, i) if i % (analog_F_sample / digital_clock_F).floor == 0 => e}
  Q = Q.zipWithIndex.collect {case (e, i) if i % (analog_F_sample / digital_clock_F).floor == 0 => e}


  it should "Fuzz Delay Chain" in {
    test(new HilbertFilter()).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.clock.step(10)
    }
  }
}
