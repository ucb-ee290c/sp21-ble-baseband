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
import chisel3.experimental.FixedPoint
import net.sparja.syto.filter.{TransferFunctionBuilder, filterForward}

import scala.collection.mutable.ListBuffer
import scala.util.Random

import breeze.stats.distributions.Gaussian
import breeze.plot._

import verif._

class DemodulationTestModule(params: BLEBasebandModemParams) extends Module {
  val io = IO(new Bundle {
    val analog = new Bundle {
      val i = Flipped(Decoupled(UInt(params.adcBits.W)))
      val q = Flipped(Decoupled(UInt(params.adcBits.W)))
    }
    val digital = new Bundle {
      val outF0 = Decoupled(UInt(12.W))
      val outF1 = Decoupled(UInt(12.W))
    }
  })

  val imageRejection = Module (new HilbertFilter(params))
  imageRejection.io.in.i.valid := io.analog.i.valid
  imageRejection.io.in.q.valid := io.analog.q.valid
  imageRejection.io.in.i.bits := io.analog.i.bits
  imageRejection.io.in.q.bits := io.analog.q.bits
  imageRejection.io.control.operation := 0.B
  io.analog.i.ready := imageRejection.io.in.i.ready
  io.analog.q.ready := imageRejection.io.in.q.ready

  val bandpassF0 = Module( new GenericFIR(FixedPoint(7.W, 0.BP), FixedPoint(20.W, 11.BP),
    FIRCoefficients.GFSKRX_Bandpass_F0.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))) )

  val bandpassF1 = Module( new GenericFIR(FixedPoint(7.W, 0.BP), FixedPoint(20.W, 11.BP),
    FIRCoefficients.GFSKRX_Bandpass_F1.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))) )

  imageRejection.io.out.ready := bandpassF0.io.in.ready && bandpassF1.io.in.ready

  bandpassF0.io.in.bits.data := imageRejection.io.out.bits.asSInt().asFixedPoint(0.BP)
  bandpassF0.io.in.valid := imageRejection.io.out.valid

  bandpassF1.io.in.bits.data := imageRejection.io.out.bits.asSInt().asFixedPoint(0.BP)
  bandpassF1.io.in.valid := imageRejection.io.out.valid

  val envelopeDetectorF0 = Module( new EnvelopeDetector(9) )
  val envelopeDetectorF1 = Module( new EnvelopeDetector(9) )

  envelopeDetectorF0.io.in.valid := bandpassF0.io.out.valid
  envelopeDetectorF0.io.in.bits := Utility.roundTowardsZero(bandpassF0.io.out.bits.data)
  bandpassF0.io.out.ready := envelopeDetectorF0.io.in.ready

  envelopeDetectorF1.io.in.valid := bandpassF1.io.out.valid
  envelopeDetectorF1.io.in.bits := Utility.roundTowardsZero(bandpassF1.io.out.bits.data)
  bandpassF1.io.out.ready := envelopeDetectorF1.io.in.ready

  envelopeDetectorF0.io.out.ready := io.digital.outF0.ready
  envelopeDetectorF1.io.out.ready := io.digital.outF1.ready
  io.digital.outF0.bits := envelopeDetectorF0.io.out.bits
  io.digital.outF1.bits := envelopeDetectorF1.io.out.bits
  io.digital.outF0.valid := envelopeDetectorF0.io.out.valid
  io.digital.outF1.valid := envelopeDetectorF1.io.out.valid
}

class GFSKDemodulationTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "Inspect Envelope Detection Waveforms" in {
    test(new DemodulationTestModule(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val numberOfBytes = 2
      val accessAddress = scala.util.Random.nextInt.abs
      val bits = Seq(0,0,0,0,0,0) ++ TestUtility.packet(accessAddress, numberOfBytes)._1 ++ Seq(0,0,0,0,0,0,0)
      val input = TestUtility.testWaveform(bits)
      val initialPhaseOffset = Random.nextInt(20)
      c.clock.step(initialPhaseOffset) // random phase offset
      var retrievedF0 = Seq[Int]()
      var retrievedF1 = Seq[Int]()
      c.io.digital.outF0.ready.poke(1.B)
      c.io.digital.outF1.ready.poke(1.B)
      for (i <- input) {
        c.io.analog.i.bits.poke((i._1).U(5.W))
        c.io.analog.i.valid.poke(1.B)

        c.io.analog.q.bits.poke((i._2).U(5.W))
        c.io.analog.q.valid.poke(1.B)

        c.clock.step()

        if (c.io.digital.outF0.valid.peek().litToBoolean && c.io.digital.outF1.valid.peek().litToBoolean) {
          retrievedF0 = retrievedF0 ++ Seq(c.io.digital.outF0.bits.peek().litValue().toInt)
          retrievedF1 = retrievedF1 ++ Seq(c.io.digital.outF1.bits.peek().litValue().toInt)
        }
      }

      val f = Figure()
      val p = f.subplot(0)
      p += plot(Seq.tabulate(retrievedF0.size)(i => i), retrievedF0, colorcode = "r")
      p += plot(Seq.tabulate(retrievedF1.size)(i => i), retrievedF1, colorcode = "b")
    }
  }
}