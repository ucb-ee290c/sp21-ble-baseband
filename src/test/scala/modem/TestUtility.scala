package modem

import breeze.plot.{Figure, plot}
import breeze.stats.distributions.Gaussian
import net.sparja.syto.filter.{TransferFunctionBuilder, filterForward}

import scala.collection.immutable.Seq
import scala.collection.mutable.ListBuffer
import scala.util.Random

object TestUtility {
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

  def centerInSupplies(in: Seq[(Double, Double)]): Seq[(Double, Double)] = {
    val minI = in.map{_._1}.min
    val maxI = in.map{_._1}.max
    val minQ = in.map{_._2}.min
    val maxQ = in.map{_._2}.max
    in.map{ s => ((s._1 - minI) / (maxI - minI) * 0.9, (s._2 - minQ) / (maxQ - minQ) * 0.9)}
  }

  def RFtoIF(in: Seq[Double], Fc: Double, imageIn: Seq[Double] = Seq(), imageFc: Double = F_IM, modulationIndex: Double = 0.5): Seq[(Double, Double)] = {
    val timeSteps = Seq.tabulate[Double]((analog_F_sample * symbol_time * (in.length / 10)).toInt)(_ * (1/(analog_F_sample)))
    val modulationFrequencyDeviation = modulationIndex / (2 * symbol_time)
    println("Modulation Index: ", modulationIndex, "Modulation Frequency Deviation: ", modulationFrequencyDeviation)
    val frequencies = timeSteps.map {t => Fc + in((t / (symbol_time / 10)).floor.toInt) * modulationFrequencyDeviation }
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
    centerInSupplies(analogLowpass(I, analog_F_sample, 10 * MHz) zip analogLowpass(Q, analog_F_sample, 10 * MHz))
  }
  def analogToDigital(in: Seq[(Double, Double)], adcBits: Int = 5): Seq[(Int, Int)] = {
    val sampled = in.zipWithIndex.collect {case (e,i) if (i % (analog_F_sample / digital_clock_F).toInt) == 0 => e}

    val range = math.pow(2, adcBits) - 1
    val scaleWeight = range / 0.9

    sampled.map{ case (i, q) => (Math.round(i * scaleWeight).toInt, Math.round(q * scaleWeight).toDouble.toInt)}
  }

  def testWaveform(bits: Seq[Int], centerFrequency: Double = F_RF): (Seq[(Int, Int)]) = {
    val imageBits = Seq.tabulate(bits.size) {_ => Random.nextInt(2)}
    val modulationIndex = 0.45 + Random.nextDouble / 10
    analogToDigital(RFtoIF(FIR(bitstream(bits), gaussian_weights), centerFrequency, modulationIndex = modulationIndex))
  }

  def noisyTestWaveform(bits: Seq[Int], centerFrequency: Double = F_RF, snr: Double = 10.0): Seq[(Int, Int)] = {
    val imageBits = Seq.tabulate(bits.size) {_ => Random.nextInt(2)}
    val cleanSignal = RFtoIF(FIR(bitstream(bits), gaussian_weights), centerFrequency)

    val noiseGen = Gaussian(0, 0.45 / Math.pow(10, snr / 20))

    val noise = noiseGen.sample(cleanSignal.length).zip(noiseGen.sample(cleanSignal.length))

    val noisySignal = cleanSignal.zip(noise).map { case ((i, q), (iNoise, qNoise)) =>
      (i + iNoise, q + qNoise)
    }.map { case (iNoisy, qNoisy) => // Constrain to voltage range 0 -> 0.9
      (math.max(0, math.min(0.9, iNoisy)), math.max(0, math.min(0.9, qNoisy)))
    }

    val f = Figure()
    val p = f.subplot(0)
    val range = cleanSignal.length
    //val range = 100
//    p += plot(Seq.tabulate(range)(i => i.toDouble), cleanSignal.map { case (i, _) => i }.take(range))
//    p += plot(Seq.tabulate(range)(i => i.toDouble), noisySignal.map { case (i, _) => i }.take(range))
//    p += plot(Seq.tabulate(range)(i => i.toDouble), cleanSignal.map { case (_, q) => q }.take(range))
//    p += plot(Seq.tabulate(range)(i => i.toDouble), noisySignal.map { case (_, q) => q }.take(range))
//    f.saveas(s"SignalPlot${snr}.png")

    analogToDigital(noisySignal) // Quantize clipped noisy signal
  }

  def addNoiseToCleanSignal(cleanSignal: Seq[(Double, Double)], snr: Double = 10.0): Seq[(Int, Int)] = {
    val noiseGen = Gaussian(0, 0.45 / Math.pow(10, snr / 20))

    val noise = noiseGen.sample(cleanSignal.length).zip(noiseGen.sample(cleanSignal.length))

    val noisySignal = cleanSignal.zip(noise).map { case ((i, q), (iNoise, qNoise)) =>
      (i + iNoise, q + qNoise)
    }.map { case (iNoisy, qNoisy) => // Constrain to voltage range 0 -> 0.9
      (math.max(0, math.min(0.9, iNoisy)), math.max(0, math.min(0.9, qNoisy)))
    }

    analogToDigital(noisySignal) // Quantize clipped noisy signal
  }

  def crc(bits: Seq[Int]): Seq[Int] = {
    var lfsr = Seq.tabulate(6){i => Seq(0, 1, 0, 1)}.flatten.reverse
    def genCRC(b: Int) = {
      val last = lfsr.last
      lfsr = lfsr.indices.map { i =>
        val xor = lfsr.last ^ b
        if (i == 0) {
          xor
        } else if (i == 1) {
          xor ^ lfsr(0)
        } else if (i == 3) {
          xor ^ lfsr(2)
        } else if (i == 4) {
          xor ^ lfsr(3)
        } else if (i == 6) {
          xor ^ lfsr(5)
        } else if (i == 9) {
          xor ^ lfsr(8)
        } else if (i == 10) {
          xor ^ lfsr(9)
        } else {
          lfsr(i - 1)
        }
      }
    }
    bits.toList.foreach {genCRC(_)}
    lfsr.reverse
  }

  def packet(aa: Int, bytes: Int): (Seq[Int], Seq[Int]) = {
    val aaLSB = aa & 0x1
    val preamble = Seq.tabulate(8){i => if (i % 2 == aaLSB) 0 else 1}
    val accessAddress = Seq.tabulate(32){i => (aa >> i) & 0x1}
    val pduLength = bytes
    val header = Seq.tabulate(8){i => 0} ++ Seq.tabulate(8){i => (pduLength >> i) & 0x1}
    val pdu = header ++ Seq.tabulate(pduLength * 8){_ => Random.nextInt(2)}
    (preamble ++ accessAddress ++ whiten(pdu ++ crc(pdu)), pdu)
  }
}
