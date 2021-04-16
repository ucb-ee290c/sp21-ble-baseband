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

class GFSKRXTestModule(params: BLEBasebandModemParams) extends Module {
  val io = IO(new Bundle {
    val analog = new Bundle {
      val i = Flipped(Decoupled(UInt(params.adcBits.W)))
      val q = Flipped(Decoupled(UInt(params.adcBits.W)))
    }
    val digital = new Bundle {
      val out = Decoupled(UInt(1.W))
    }
  })
  val gfskRX = Module(new GFSKRX(params)).io

  val preambleDetected = gfskRX.control.out.preambleDetected
  gfskRX.control.in.imageRejectionOp := 0.B
  gfskRX.control.in.enable := 1.B
  gfskRX.control.in.accessAddressLSB := 1.U
  gfskRX.control.in.preambleDetectionThreshold := 140.U
  gfskRX.analog <> io.analog
  io.digital.out.bits := gfskRX.digital.out.bits
  io.digital.out.valid := preambleDetected & gfskRX.digital.out.valid
  gfskRX.digital.out.ready := io.digital.out.ready
}

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


class GFSKRXTest extends AnyFlatSpec with ChiselScalatestTester {
/*
  it should "Determine SNR vs BER" in {
    val numberOfBits = 50
    val preamble = Seq(1,0,1,0,1,0,1,0)

    val SNRvBER = Seq.tabulate(11)(i => {
      val noiseAmplitude = i.toDouble / 100
      val SNR = 1 / noiseAmplitude
      var BER = 2.0
      println(s"Testing SNR of ${SNR}:")

      test(new GFSKRXTestModule(BLEBasebandModemParams())).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
        val inDriverI = new DecoupledDriverMaster(c.clock, c.io.analog.i)
        val inDriverQ = new DecoupledDriverMaster(c.clock, c.io.analog.q)
        val outDriver = new DecoupledDriverSlave(c.clock, c.io.digital.out)
        val outMonitor = new DecoupledMonitor(c.clock, c.io.digital.out)

        val packet = Seq.tabulate(numberOfBits){_ => Random.nextInt(2)}
        val bits = Seq(0,0,0,0,0,0) ++ preamble ++ whiten(packet) ++ Seq(0,0,0,0,0,0,0)
        val input = noisyTestWaveform(bits, noiseAmplitude = noiseAmplitude)
        val initialPhaseOffset = Random.nextInt(20)
        //c.clock.step(initialPhaseOffset) // random phase offset

        inDriverI.push(input.map(p => new DecoupledTX(UInt(5.W)).tx(p._1.U(5.W))))
        inDriverQ.push(input.map(p => new DecoupledTX(UInt(5.W)).tx(p._2.U(5.W))))
        c.clock.step(bits.size * 20)

        val retrieved = whiten(outMonitor.monitoredTransactions.map{_.data.litValue.toInt})
        //println("Initial Phase Offset: ",initialPhaseOffset, "Received Data: ", retrieved, "Expected Data: ", packet)
        assert(retrieved.size > 0)
        BER = 1.0 - ((packet.zip(retrieved).count { case (observed, expected) => observed == expected }).toDouble / packet.length)

        println((SNR, BER))
      }

      (SNR, BER)
    })

    println(SNRvBER)
  }
*/
  it should "PASS Fuzz" in {
    test(new GFSKRXTestModule(new BLEBasebandModemParams())).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val inDriverI = new DecoupledDriverMaster(c.clock, c.io.analog.i)
      val inDriverQ = new DecoupledDriverMaster(c.clock, c.io.analog.q)
      val outDriver = new DecoupledDriverSlave(c.clock, c.io.digital.out)
      val outMonitor = new DecoupledMonitor(c.clock, c.io.digital.out)
      val numberOfBits = 50
      val preamble = Seq(1,0,1,0,1,0,1,0)
      val packet = Seq.tabulate(numberOfBits){_ => Random.nextInt(2)}
      val bits = Seq(0,0,0,0,0,0) ++ preamble ++ TestUtility.whiten(packet) ++ Seq(0,0,0,0,0,0,0)
      val input = TestUtility.testWaveform(bits)
      val initialPhaseOffset = Random.nextInt(20)
      c.clock.step(initialPhaseOffset) // random phase offset
      inDriverI.push(input.map(p => new DecoupledTX(UInt(5.W)).tx(p._1.U(5.W))))
      inDriverQ.push(input.map(p => new DecoupledTX(UInt(5.W)).tx(p._2.U(5.W))))
      c.clock.step(bits.size * 20)

      val retrieved = TestUtility.whiten(outMonitor.monitoredTransactions.map{_.data.litValue.toInt})
      println("Initial Phase Offset: ",initialPhaseOffset, "Received Data: ", retrieved, "Expected Data: ", packet)
      assert(retrieved.size > 0 && packet.zip(retrieved).forall {p => p._1 == p._2})
    }
  }

  it should "PASS Radio Frequency" in {
    test(new HilbertFilter(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.out.ready.poke(1.B)
      var arr = Seq[BigInt]()
      var i = 0
      val numberOfBits = 20
      val preamble = Seq(1,0,1,0,1,0,1,0)
      val packet = Seq.tabulate(numberOfBits){_ => Random.nextInt(2)}
      val bits = Seq(0,0,0,0,0,0) ++ preamble ++ TestUtility.whiten(packet) ++ Seq(0,0,0,0,0,0,0)
      val input = TestUtility.testWaveform(bits)
      while (i < input.length) {
        c.io.in.i.bits.poke(input(i)._1.asUInt())
        c.io.in.q.bits.poke(input(i)._2.asUInt())
        c.io.in.i.valid.poke((i < input.length).asBool())
        c.io.in.q.valid.poke((i < input.length).asBool())
        c.clock.step()
       if (c.io.out.valid.peek().litToBoolean)
          arr = arr ++ Seq(c.io.out.bits.peek().litValue())
        i+=1
      }
      print("Output:\n")
      print(arr)
      val f = Figure()
      val p = f.subplot(0)
      p += plot(Seq.tabulate(arr.size)(i => i), arr.map {_.toInt}, colorcode = "b")
      assert(true)
    }
  }

  it should "REJECT IMAGE" in {
    test(new HilbertFilter(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.out.ready.poke(1.B)
      var arr = Seq[BigInt]()
      var i = 0
      val numberOfBits = 20
      val preamble = Seq(1,0,1,0,1,0,1,0)
      val packet = Seq.tabulate(numberOfBits){_ => Random.nextInt(2)}
      val bits = Seq(0,0,0,0,0,0) ++ preamble ++ TestUtility.whiten(packet) ++ Seq(0,0,0,0,0,0,0)
      val input = TestUtility.testWaveform(bits, TestUtility.F_IM)
      while (i < input.length) {
        c.io.in.i.bits.poke(input(i)._1.asUInt())
        c.io.in.q.bits.poke(input(i)._2.asUInt())
        c.io.in.i.valid.poke((i < input.length).asBool())
        c.io.in.q.valid.poke((i < input.length).asBool())
        c.clock.step()
       if (c.io.out.valid.peek().litToBoolean)
          arr = arr ++ Seq(c.io.out.bits.peek().litValue())
        i+=1
      }
      print("Output:\n")
      print(arr)
      val f = Figure()
      val p = f.subplot(0)
      p += plot(Seq.tabulate(arr.size)(i => i), arr.map {_.toInt}, colorcode = "r")
      assert(true)
    }
  }

  it should "Bandpass Appropriately" in {
    test(new DemodulationTestModule(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val numberOfBits = 50
      val preamble = Seq(1,0,1,0,1,0,1,0)
      val packet = Seq.tabulate(numberOfBits){_ => Random.nextInt(2)}
      val bits = Seq(0,0,0,0,0,0) ++ preamble ++ TestUtility.whiten(packet) ++ Seq(0,0,0,0,0,0,0)
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
