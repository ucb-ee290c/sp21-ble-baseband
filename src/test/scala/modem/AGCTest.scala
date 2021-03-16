package modem

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, TreadleBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

import baseband.BLEBasebandModemParams

class AGCTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "Elaborate an AGC" in {
    test(new AGC(BLEBasebandModemParams())).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.adcIn.valid.poke(true.B)
      for (i <- 0 until 10) {
        c.io.adcIn.bits.poke(i.U)
        println(c.io.vgaAttenuation.peek())
        c.clock.step()
      }
    }
  }
}