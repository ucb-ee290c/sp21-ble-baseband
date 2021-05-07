package modem

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

import scala.collection.immutable.Seq
import TestUtility._

import scala.math.{max, min}
import baseband.BLEBasebandModemParams
import breeze.plot.{Figure, plot}

class AGCTest extends AnyFlatSpec with ChiselScalatestTester {
  def vgaLUT(index: Int): Int = {
    index match {
      case 0 => 7 // Centered
      case 1 => 6
      case 2 => 6
      case 3 => 5
      case 4 => 5
      case 5 => 4
      case 6 => 4
      case 7 => 3
      case 8 => 3
      case 9 => 2
      case 10 => 2
      case 11 => 1
      case 12 => 1
      case 13 => 0
      case 14 => 0
      case 15 => 0
      case 16 => 15 // -16
      case 17 => 15 // -15
      case 18 => 14 // -14
      case 19 => 14 // -13
      case 20 => 13 // -12
      case 21 => 13 // -11
      case 22 => 12 // -10
      case 23 => 12 // -9
      case 24 => 11 // -8
      case 25 => 11 // -7
      case 26 => 10 // -6
      case 27 => 10 // -5
      case 28 => 9  // -4
      case 29 => 9  // -3
      case 30 => 8  // -2
      case 31 => 8  // -1
      case _ => -1
    }
  }

  it should "Test AGC Convergence" in {
    test(new AGC(BLEBasebandModemParams())).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.control.idealPeakToPeak.poke(128.U)
      c.io.control.sampleWindow.poke(1.U)
      c.io.control.gain.poke((0.75).F(8.W, 6.BP))
      c.clock.step()

      val numberOfBytes = 1
      val accessAddress = scala.util.Random.nextInt.abs
      val packet = TestUtility.packet(accessAddress, numberOfBytes)._1
      val bits = Seq(0,0,0,0,0,0) ++ packet ++ Seq(0,0,0,0,0,0,0)

      val quantizationWeight = (math.pow(2, BLEBasebandModemParams().adcBits) - 1) / 0.9

      val preVGAAmplitude = 0.0001
      val samples = centerInSupplies(fastIF(FIR(bitstream(bits), gaussian_weights)))
        .map{ case (i, _) => i * preVGAAmplitude }


      c.io.adcIn.valid.poke(true.B)

      val postVGASample = samples.map {case sample =>
        val lutIndex = c.io.vgaLUTIndex.peek().litValue().toInt
        val vgaAmplitude = vgaGain(vgaLUT(lutIndex))
        val scaledSample = max(min(0.9, sample * vgaAmplitude), 0) // Scale sample and constrain within rails
        val quantizedSample = Math.round(scaledSample * quantizationWeight).toInt // Quantize
        c.io.adcIn.bits.poke(quantizedSample.U)
        c.clock.step()
        (scaledSample, vgaAmplitude, lutIndex, quantizedSample)
      }
      println(postVGASample.map(s => s._2))
      val f = Figure()
      val p = f.subplot(0)
      val range = postVGASample.length
      //p += plot(Seq.tabulate(range)(i => i.toDouble), postVGASample.map(s => s._1).take(range))
      p += plot(Seq.tabulate(range)(i => i.toDouble), postVGASample.map(s => s._2).take(range))
      //p += plot(Seq.tabulate(range)(i => i), postVGASample.map(s =>  -1*(s._3 & 16) + (s._3 & 15)).take(range))
      //p += plot(Seq.tabulate(range)(i => i), postVGASample.map(s => s._4).take(range))

//      val f1 = Figure()
//      val p1 = f1.subplot(0)
//      p1 += plot(Seq.tabulate(range)(i => i.toDouble), samples.take(range))
    }
  }
}