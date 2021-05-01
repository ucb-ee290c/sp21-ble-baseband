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
      val outF0 = Decoupled(UInt(15.W))
      val outF1 = Decoupled(UInt(15.W))
    }
  })

  val imageRejection = Module (new HilbertFilter(params))
  imageRejection.io.in.i.valid := io.analog.i.valid
  imageRejection.io.in.q.valid := io.analog.q.valid
  imageRejection.io.in.i.bits := io.analog.i.bits
  imageRejection.io.in.q.bits := io.analog.q.bits
  imageRejection.io.control.operation := 0.B
  imageRejection.io.control.IonLHS := 0.B
  imageRejection.io.control.IonTop := 0.B
  io.analog.i.ready := imageRejection.io.in.i.ready
  io.analog.q.ready := imageRejection.io.in.q.ready
  imageRejection.io.filterCoeffCommand.valid := 0.B
  imageRejection.io.filterCoeffCommand.bits.FIR := 0.U
  imageRejection.io.filterCoeffCommand.bits.change.coeff := 0.U
  imageRejection.io.filterCoeffCommand.bits.change.value := 0.U


  val bandpassF0Width = FIRCoefficients.GFSKRX_Bandpass_F0.width
  val bandpassF0BP = FIRCoefficients.GFSKRX_Bandpass_F0.binaryPoint
  val bandpassF0Coeffs = RegInit(VecInit(FIRCoefficients.GFSKRX_Bandpass_F0.coefficients.map(c => FixedPoint.fromDouble(c, bandpassF0Width.W, bandpassF0BP.BP))))

  /* Bandpass Filters for 0 and 1 frequencies */
  val bandpassF0 = Module(
    new FixedPointTransposeFIR(
      FixedPoint(10.W, 0.BP),
      FixedPoint((10 + 1 + bandpassF0Width).W, bandpassF0BP.BP),
      FixedPoint(bandpassF0Width.W, bandpassF0BP.BP),
      FIRCoefficients.GFSKRX_Bandpass_F0.coefficients.length
    )
  )
  bandpassF0.io.coeff := bandpassF0Coeffs

  val bandpassF1Width = FIRCoefficients.GFSKRX_Bandpass_F1_ALT.width
  val bandpassF1BP = FIRCoefficients.GFSKRX_Bandpass_F1_ALT.binaryPoint
  val bandpassF1Coeffs = RegInit(VecInit(FIRCoefficients.GFSKRX_Bandpass_F1_ALT.coefficients.map(c => FixedPoint.fromDouble(c, bandpassF1Width.W, bandpassF1BP.BP))))

  val bandpassF1 = Module(
    new FixedPointTransposeFIR(
      FixedPoint(10.W, 0.BP),
      FixedPoint((10 + 1 + bandpassF1Width).W, bandpassF1BP.BP),
      FixedPoint(bandpassF1Width.W, bandpassF1BP.BP),
      FIRCoefficients.GFSKRX_Bandpass_F1_ALT.coefficients.length
    )
  )
  bandpassF1.io.coeff := bandpassF1Coeffs

  imageRejection.io.out.ready := bandpassF0.io.in.ready && bandpassF1.io.in.ready

  bandpassF0.io.in.bits := imageRejection.io.out.bits.asSInt().asFixedPoint(0.BP)
  bandpassF0.io.in.valid := imageRejection.io.out.valid

  bandpassF1.io.in.bits := imageRejection.io.out.bits.asSInt().asFixedPoint(0.BP)
  bandpassF1.io.in.valid := imageRejection.io.out.valid

  val envelopeDetectorWidth = FIRCoefficients.GFSKRX_Envelope_Detector.width
  val envelopeDetectorBP = FIRCoefficients.GFSKRX_Envelope_Detector.binaryPoint
  val envelopeDetectorCoeffs = RegInit(VecInit(FIRCoefficients.GFSKRX_Envelope_Detector.coefficients.map(c => FixedPoint.fromDouble(c, envelopeDetectorWidth.W, envelopeDetectorBP.BP))))

  /* Envelope Detectors. */
  val envelopeDetectorF0 = Module( new EnvelopeDetector(bandpassF0.io.out.bits.getWidth - bandpassF0.io.out.bits.binaryPoint.get) )
  val envelopeDetectorF1 = Module( new EnvelopeDetector(bandpassF1.io.out.bits.getWidth - bandpassF1.io.out.bits.binaryPoint.get) )

  envelopeDetectorF0.io.in.valid := bandpassF0.io.out.valid
  envelopeDetectorF0.io.in.bits := Utility.roundTowardsZero(bandpassF0.io.out.bits)
  bandpassF0.io.out.ready := envelopeDetectorF0.io.in.ready
  envelopeDetectorF0.io.coeffs := envelopeDetectorCoeffs

  envelopeDetectorF1.io.in.valid := bandpassF1.io.out.valid
  envelopeDetectorF1.io.in.bits := Utility.roundTowardsZero(bandpassF1.io.out.bits)
  bandpassF1.io.out.ready := envelopeDetectorF1.io.in.ready
  envelopeDetectorF1.io.coeffs := envelopeDetectorCoeffs

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
      val input = TestUtility.testWaveform(bits, imageAmplitude = 1)
      val initialPhaseOffset = Random.nextInt(20)
      c.clock.step(initialPhaseOffset) // random phase offset
      var retrievedF0 = Seq[Int]()
      var retrievedF1 = Seq[Int]()
      c.io.digital.outF0.ready.poke(1.B)
      c.io.digital.outF1.ready.poke(1.B)
      for (i <- input) {
        c.io.analog.i.bits.poke((i._1).U(8.W))
        c.io.analog.i.valid.poke(1.B)

        c.io.analog.q.bits.poke((i._2).U(8.W))
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
