package modem

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, TreadleBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

import verif._

import baseband.BLEBasebandModemParams

class EnvelopeDetectorTest extends AnyFlatSpec with ChiselScalatestTester {
  val f = 2000000 // 2 MHz IF
  val fs = 20000000 // 20 MHz Sampling Frequency
  val amplitude = 1

  it should "Test an Envelope Detector" in {
    val bitWidth = 8
    test(new EnvelopeDetector(8)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val inDriver = new DecoupledDriverMaster(c.clock, c.io.in)
      val outDriver = new DecoupledDriverSlave(c.clock, c.io.out)
      val outMonitor = new DecoupledMonitor(c.clock, c.io.out)

      val offset = -1000000 // Total signal at 1 MHz, should passed
      val samplePoints = Seq.tabulate(500)(i =>
        math.round((amplitude * scala.math.sin(2 * scala.math.Pi * (f + offset) * (i.toDouble / fs))) * (math.pow(2, bitWidth - 1) - 1)))

      inDriver.push(samplePoints.map(p => new DecoupledTX(SInt(bitWidth.W)).tx(p.S(bitWidth.W))))

      c.clock.step(600)

      println(outMonitor.monitoredTransactions)
    }
  }
}