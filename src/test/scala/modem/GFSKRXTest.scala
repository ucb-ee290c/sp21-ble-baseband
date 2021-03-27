package modem

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import collection.immutable.Seq
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec
import baseband.BLEBasebandModemParams
import net.sparja.syto.filter.{filterForward, TransferFunctionBuilder}



class GFSKRXTest extends AnyFlatSpec with ChiselScalatestTester {

  val MHz = 1000000

  val channel_index = 0
  val F_RF = (2402 + 2 * channel_index) * MHz
  val F_IF = 2 * MHz
  val F_LO = F_RF - F_IF
  val F_IM = F_LO - F_IF
  val analog_F_sample = (F_LO * 2 + F_IF) * 2 + (0.5 * MHz)
  val time_interval = 0.0000001
  val symbol_time = 0.000001
  val symbol_num = 10
  val digital_clock_F = 20 * MHz

  def analogLowpass(signal: Seq[Double], Fs: Double, Fc: Double): Seq[Double] = {
    val (b, a) = new TransferFunctionBuilder()
      .butterworthApproximation(5)
      .digitalize(Fs)
      .transformToLowPass(Fc) // Low-pass filter with cutoff frequency 3.5Hz
      .coefficients
    filterForward(b, a, signal)
  }

  def RFtoIF(in: Seq[Double]): (Seq[Double], Seq[Double]) = {
    val timeSteps = Seq.tabulate[Double]((analog_F_sample * symbol_time * symbol_num).toInt)(_ * (1/(analog_F_sample)))
    val rf = {t:Double => math.cos(2 * math.Pi * F_RF * t + math.Pi / 4)}
    val I = {t:Double => rf(t) * math.cos(2 * math.Pi * F_LO * t)}
    val Q = {t:Double => rf(t) * math.sin(2 * math.Pi * F_LO * t)}
    return (analogLowpass(timeSteps.map{I}, analog_F_sample, 10 * MHz).zipWithIndex.collect {case (e,i) if (i % (analog_F_sample / digital_clock_F).toInt) == 0 => e},
      analogLowpass(timeSteps.map{Q}, analog_F_sample, 10 * MHz).zipWithIndex.collect {case (e,i) if (i % (analog_F_sample / digital_clock_F).toInt) == 0 => e})
  }

  it should "Elaborate a modem" in {
    test(new GFSKRX(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.clock.step(10)
      print(RFtoIF(Seq())._1)
    }
  }
}
