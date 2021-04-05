package modem

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, TreadleBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

import baseband.BLEBasebandModemParams

class FixedPointTester extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(8.W))
    val subIn1 = Input(UInt(2.W))
    val subIn2 = Input(UInt(2.W))
    val outPos = Output(FixedPoint(16.W, 6.BP))
    val outNeg = Output(FixedPoint(16.W, 6.BP))
    val subTest = Output(UInt(3.W))
  })

  println(io.in.asFixedPoint(0.BP) * (1.99).F(8.W, 6.BP))
  io.outPos := io.in.asFixedPoint(0.BP) * (1.99).F(8.W, 6.BP)
  io.outNeg := io.in.asFixedPoint(0.BP) * (-1.99).F(8.W, 6.BP)
  io.subTest := Cat(0.U(1.W), io.subIn1) - Cat(0.U(1.W), io.subIn2)
}

class AGCTest extends AnyFlatSpec with ChiselScalatestTester {
  val f = 2000000 // 2 MHz IF
  val fs = 20000000 // 20 MHz Sampling Frequency

  it should "Elaborate an AGC" in {
    test(new AGC(BLEBasebandModemParams())).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.control.idealPeakToPeak.poke(128.U)
      c.io.control.sampleWindow.poke(1.U)
      c.io.control.gain.poke((1.0).F(8.W, 6.BP))
      c.io.adcIn.valid.poke(true.B)

      var amplitudes = Seq[Double]()
      var amplitude = 1.5
      for (i <- 0 until 500) {
        val sinPoint = (amplitude * scala.math.sin(2 * scala.math.Pi * f * (i.toDouble / fs))) + 1
        val sample = if (sinPoint > 2) {
          255
        } else if (sinPoint < 0) {
          0
        } else {
          scala.math.round(sinPoint * 127.5)
        }
        c.io.adcIn.bits.poke(sample.U)
        c.clock.step()
        val lutIndex = c.io.vgaLUTIndex.peek().litValue().toInt
        val signedLUTIndex = (lutIndex & 0xF) - (lutIndex & 0x10)
        amplitude = amplitude * (1 - (math.signum(signedLUTIndex) * ((signedLUTIndex * signedLUTIndex * 0.1)/256)))
        amplitudes = amplitudes :+ amplitude
      }

      // Jump signal strength down
      amplitude = 0.1
      for (i <- 0 until 500) {
        val sinPoint = (amplitude * scala.math.sin(2 * scala.math.Pi * f * (i.toDouble / fs))) + 1
        val sample = if (sinPoint > 2) {
          255
        } else if (sinPoint < 0) {
          0
        } else {
          scala.math.round(sinPoint * 127.5)
        }
        c.io.adcIn.bits.poke(sample.U)
        c.clock.step()
        val lutIndex = c.io.vgaLUTIndex.peek().litValue().toInt
        val signedLUTIndex = (lutIndex & 0xF) - (lutIndex & 0x10)
        amplitude = amplitude * (1 - (math.signum(signedLUTIndex) * ((signedLUTIndex * signedLUTIndex * 0.1)/256)))
        amplitudes = amplitudes :+ amplitude
      }

      println(amplitudes)
    }
  }

  it should "Test UInt to Fixed Point" in {
    test(new FixedPointTester).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.in.poke(255.U)
      c.io.subIn1.poke(2.U)
      c.io.subIn2.poke(3.U)
      c.clock.step()
      c.io.subIn1.poke(1.U)
      c.io.subIn2.poke(3.U)
      c.clock.step()
      c.io.subIn1.poke(3.U)
      c.io.subIn2.poke(1.U)
      c.clock.step()
    }
  }
}