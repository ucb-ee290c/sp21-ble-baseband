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
      c.io.control.idealPeakToPeak.poke(121.U)
      c.io.control.sampleWindow.poke(1.U)
      c.io.control.gain.poke((0.99).F(8.W, 6.BP))

      for (amplitude <- Seq(0.5, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1, 1.1, 1.2, 1.3, 1.4, 1.5)) {
        println(s"Amplitude: $amplitude")
        c.io.adcIn.valid.poke(true.B)
        for (i <- 0 until 40) {
          val sinPoint = (amplitude * scala.math.sin(2 * scala.math.Pi * f * (i.toDouble / fs))) + 1
          val sample = if (sinPoint > 2) {
            255
          } else if (sinPoint < 0) {
            0
          } else {
            scala.math.round(sinPoint * 127.5)
          }
          println(sample)
          c.io.adcIn.bits.poke(sample.U)
          c.clock.step()
        }
        c.io.adcIn.valid.poke(false.B)
        c.clock.step()
      }
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