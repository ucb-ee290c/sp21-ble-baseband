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
    test(new CDRDecision(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val sample = Seq(31, 25, 10, -10, -25, -31, -25, -10, 10, 25, 30, 25, 10, -10, -25, -31, -25, -10, 9, 25, 30, 25, 10, -9, -24, -31, -27, -13, 5, 21, 30, 29, 19, 3, -14, -26, -31, -26, -13, 4, 19, 29, 30, 21, 5, -12, -26, -31, -24, -8, 12, 27, 30, 21, 2, -18, -30, -28, -13, 8, 25, 30, 22, 4, -16, -29, -30, -18, 0, 18, 29, 30, 20, 4, -13, -26, -31, -26, -13, 4, 19, 29, 30, 21, 5, -12, -26, -31, -24, -8, 12, 27, 30, 21, 2, -18, -30, -28, -13, 8, 25, 30, 22, 4, -16, -29, -30, -18, 0, 18, 29, 30, 20, 4, -13, -26, -31, -26, -13, 4, 19, 29, 30, 21, 5, -12, -26, -31, -24, -8, 12, 27, 31, 21, 2, -18, -30, -28, -13, 8, 25, 30, 22, 4, -16, -29, -30, -18, 0, 18, 29, 30, 20, 4, -13, -26, -31, -26, -13, 4, 19, 29, 30, 21, 5, -12, -26, -31, -24, -8, 12, 27, 30, 21, 2, -18, -30, -28, -13, 8, 25, 31, 22, 4, -16, -29, -30, -18, 0, 18, 29, 30, 20, 4, -13, -26, -31, -26, -13, 4)
      val inDriver = new DecoupledDriverMaster(c.clock, c.io.signal)
      val outDriver = new DecoupledDriverSlave(c.clock, c.io.out)
      val outMonitor = new DecoupledMonitor(c.clock, c.io.out)
      inDriver.push(input.map(p => new DecoupledTX(SInt(6.W)).tx(p.S(6.W))))
      c.clock.step(1000)
      println(outMonitor.monitoredTransactions.map{_.data})
    }
  }
}
