package modem

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec
import baseband.BLEBasebandModemParams

import scala.collection.immutable.Seq
import scala.util.Random

class ModemTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "Elaborate a modem" in {
    test(new GFSKModem(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.control.rx.in.preambleDetectionThreshold.poke(140.U)
      c.io.control.rx.in.accessAddressLSB.poke(1.U)
      c.io.control.rx.in.imageRejectionControl.poke(0.U)
      c.io.control.rx.in.enable.poke(1.B)
      c.io.digital.rx.ready.poke(1.B)

      val numberOfBits = 50
      val preamble = Seq(1,0,1,0,1,0,1,0)
      val packet = Seq.tabulate(numberOfBits){_ => Random.nextInt(2)}
      val bits = Seq(0,0,0,0,0,0) ++ preamble ++ TestUtility.whiten(packet, 0) ++ Seq(0,0,0,0,0,0,0)
      val input = TestUtility.testWaveform(bits)
      val initialPhaseOffset = Random.nextInt(20)
      var retrieved = Seq[Int]()
      for (i <- input) {
        c.io.analog.rx.i.data.poke(i._1.U)
        c.io.analog.rx.q.data.poke(i._2.U)
        c.clock.step()
        if (c.io.control.rx.out.preambleDetected.peek().litToBoolean && c.io.digital.rx.valid.peek().litToBoolean) {
          retrieved = retrieved ++ Seq(c.io.digital.rx.bits.peek().litValue().toInt)
        }
      }
      print(retrieved)
    }
  }
}