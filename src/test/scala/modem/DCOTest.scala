package modem

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, TreadleBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

import baseband.BLEBasebandModemParams

class DCOTest extends AnyFlatSpec with ChiselScalatestTester {
  val f = 2000000 // 2 MHz IF
  val fs = 20000000 // 20 MHz Sampling Frequency

  it should "Elaborate a DCO" in {
    test(new DCO(BLEBasebandModemParams())).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.control.gain.poke((0.9).F(8.W, 6.BP))
      c.io.adcIn.valid.poke(true.B)

      val amplitude = 1.0
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
      }
    }
  }
}