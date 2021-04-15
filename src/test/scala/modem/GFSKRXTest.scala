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

  val MHz = 1000000

  val channel_index = 0
  val F_RF = (2402 + 2 * channel_index) * MHz
  val F_IF = 2 * MHz
  val F_LO = F_RF - F_IF
  val F_IM = F_LO - F_IF
  val analog_F_sample = (F_LO * 2 + F_IF + 0.25 * MHz) * 2
  val time_interval = 0.0000001
  val symbol_time = 0.000001
  val digital_clock_F = 20 * MHz

  def analogLowpass(signal: Seq[Double], Fs: Double, Fc: Double): Seq[Double] = {
    val (b, a) = new TransferFunctionBuilder()
      .butterworthApproximation(5)
      .digitalize(Fs)
      .transformToLowPass(Fc) // Low-pass filter with cutoff frequency 3.5Hz
      .coefficients
    filterForward(b, a, signal)
  }

  def whiten(bits: Iterable[Int]): List[Int] = {
    var lfsr = Seq(1, 0, 0, 0, 0, 0, 0)
    def whitener(b: Int): Int = {
      val last = lfsr.last
      lfsr = lfsr.indices.map { i =>

        if (i == 0) {
          lfsr.last
        } else if (i == 4) {
          lfsr.last ^ lfsr(3)
        } else {
          lfsr(i - 1)
        }
      }
      last ^ b
    }
    bits.toList.map{whitener(_)}
  }

  val gaussian_weights = Seq(1.66272941385205e-08, 1.31062698399579e-07, 8.95979722260093e-07, 5.31225368476001e-06, 2.73162439119005e-05, 0.000121821714511972, 0.000471183401324158, 0.00158058118651170, 0.00459838345240388, 0.0116025943557647, 0.0253902270288187, 0.0481880785252652, 0.0793184437320263, 0.113232294984428, 0.140193534368681, 0.150538370165906, 0.140193534368681, 0.113232294984428, 0.0793184437320263, 0.0481880785252652, 0.0253902270288187, 0.0116025943557647, 0.00459838345240388, 0.00158058118651170, 0.000471183401324158, 0.000121821714511972, 2.73162439119005e-05, 5.31225368476001e-06, 8.95979722260093e-07, 1.31062698399579e-07, 1.66272941385205e-08)
  def bitstream(in: Seq[Int]): Seq[Double] = {
    in.flatMap{b: Int => Seq.tabulate(10){i: Int => (if (b == 0) -1.0 else 1.0)}}
  }

  def FIR(in: Seq[Double], coeff: Seq[Double]): Seq[Double] = {
    var samples = ListBuffer[Double]()
    return in.map { s: Double =>
      samples.prepend(s)
      samples.zip(coeff).map {g: (Double, Double) => g._1 * g._2}.sum
    }
  }

  def RFtoIF(in: Seq[Double], Fc: Double, imageIn: Seq[Double] = Seq(), imageFc: Double = F_IM): Seq[(Double, Double)] = {
    val timeSteps = Seq.tabulate[Double]((analog_F_sample * symbol_time * (in.length / 10)).toInt)(_ * (1/(analog_F_sample)))
    val frequencies = timeSteps.map {t => Fc + in((t / (symbol_time / 10)).floor.toInt) * 0.25 * MHz }
    val phases = frequencies.map{var s: Double = 0.0; d => {s += d; 2 * math.Pi * s * (1/analog_F_sample)}}
    val rf = phases.map {math.cos(_)}
    var signal = rf
    if (imageIn.size > 0) {
      val image_frequencies = timeSteps.map {t => imageFc + imageIn((t / (symbol_time / 10)).floor.toInt) * 0.25 * MHz }
      val image_phases = image_frequencies.map{var s: Double = math.Pi / 4; d => {s += d; 2 * math.Pi * s * (1/analog_F_sample)}}
      val image = image_phases.map {v => math.cos(v)}
      signal = rf.zip(image).map {p => p._1 + p._2}
    }

    val I = timeSteps.indices.map {i => signal(i) * math.cos(2 * math.Pi * F_LO * timeSteps(i))}
    val Q = timeSteps.indices.map {i => signal(i) * math.sin(2 * math.Pi * F_LO * timeSteps(i))}
    return analogLowpass(I, analog_F_sample, 10 * MHz) zip analogLowpass(Q, analog_F_sample, 10 * MHz)
  }
  def analogToDigital(in: (Seq[(Double, Double)])): (Seq[(Int, Int)]) = {
    val sampled = in.zipWithIndex.collect {case (e,i) if (i % (analog_F_sample / digital_clock_F).toInt) == 0 => e}
    val maxI = sampled.map{_._1}.max
    val maxQ = sampled.map{_._2}.max
    val minI = sampled.map{_._1}.min
    val minQ = sampled.map{_._2}.min
    return sampled.map{s: (Double, Double) => (((s._1 - minI) / (maxI - minI) * 31).toInt, ((s._2 - minQ) / (maxQ - minQ) * 31).toInt)}
  }

  def testWaveform(bits: Seq[Int], centerFrequency: Double = F_RF): (Seq[(Int, Int)]) = {
    val imageBits = Seq.tabulate(bits.size) {_ => Random.nextInt(2)}
    analogToDigital(RFtoIF(FIR(bitstream(bits), gaussian_weights), centerFrequency))
  }

  def noisyTestWaveform(bits: Seq[Int], centerFrequency: Double = F_RF, noiseAmplitude: Double = 1.0): (Seq[(Int, Int)]) = {
    val imageBits = Seq.tabulate(bits.size) {_ => Random.nextInt(2)}
    val cleanSignal = analogToDigital(RFtoIF(FIR(bitstream(bits), gaussian_weights), centerFrequency))

    val noiseGen = Gaussian(0, 31.toDouble/6) // Gaussian with SD such that most data is between +/- 15.5
    val noise = noiseGen.sample(cleanSignal.length).zip(noiseGen.sample(cleanSignal.length))

    val noisySignal = cleanSignal.zip(noise).map { case ((i, q), (iNoise, qNoise)) =>
      (i + noiseAmplitude * iNoise, q + noiseAmplitude * qNoise)
    }.map { case (iNoisy, qNoisy) =>
      (math.max(0, math.min(31, iNoisy.round)).toInt, math.max(0, math.min(31, qNoisy.round)).toInt)
    }

    val f = Figure()
    val p = f.subplot(0)
    p += plot(Seq.tabulate(100)(i => i), cleanSignal.map { case (i, q) => i }.take(100))
    p += plot(Seq.tabulate(100)(i => i), noisySignal.map { case (i, q) => i }.take(100), colorcode = "r")
    f.saveas(s"SignalPlot${noiseAmplitude}.png")
    noisySignal
  }
/*
  it should "Determine SNR vs BER" in {
    val numberOfBits = 50
    val preamble = Seq(1,0,1,0,1,0,1,0)

    val SNRvBER = Seq.tabulate(11)(i => {
      val noiseAmplitude = i.toDouble / 10
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
      val bits = Seq(0,0,0,0,0,0) ++ preamble ++ whiten(packet) ++ Seq(0,0,0,0,0,0,0)
      val input = testWaveform(bits)
      val initialPhaseOffset = Random.nextInt(20)
      c.clock.step(initialPhaseOffset) // random phase offset
      inDriverI.push(input.map(p => new DecoupledTX(UInt(5.W)).tx(p._1.U(5.W))))
      inDriverQ.push(input.map(p => new DecoupledTX(UInt(5.W)).tx(p._2.U(5.W))))
      c.clock.step(bits.size * 20)

      val retrieved = whiten(outMonitor.monitoredTransactions.map{_.data.litValue.toInt})
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
      val bits = Seq(0,0,0,0,0,0) ++ preamble ++ whiten(packet) ++ Seq(0,0,0,0,0,0,0)
      val input = testWaveform(bits)
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
      val bits = Seq(0,0,0,0,0,0) ++ preamble ++ whiten(packet) ++ Seq(0,0,0,0,0,0,0)
      val input = testWaveform(bits, F_IM)
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
      val bits = Seq(0,0,0,0,0,0) ++ preamble ++ whiten(packet) ++ Seq(0,0,0,0,0,0,0)
      val input = testWaveform(bits)
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
