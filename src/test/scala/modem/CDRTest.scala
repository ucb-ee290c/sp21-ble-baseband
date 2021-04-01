package modem

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec
import baseband.BLEBasebandModemParams

import scala.collection.immutable.Seq

class CDRTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "Generate Waveform" in {
    test(new CDR()).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.d.poke(1.B)
      for (_ <- 1 to 10) {
        c.clock.step(1)
      }
      var sym: Int = 0
      for (sym <- 1 to 100) {
        c.io.d.poke((sym % 2 == 0).asBool())
        for (_ <- 1 to 20) {
          c.clock.step(1)
        }
      }
    }
  }
}
