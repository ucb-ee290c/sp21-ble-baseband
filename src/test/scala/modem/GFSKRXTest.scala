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
  gfskRX.control.in.imageRejectionOp := 0.B

  val preambleDetected = RegInit(0.B)
  def risingedge(x: Bool) = x && !RegNext(x)
  when (risingedge(gfskRX.control.out.preambleDetected)) {
    preambleDetected := 1.B
  }

  gfskRX.analog <> io.analog
  io.digital.out.bits := gfskRX.digital.out.bits
  io.digital.out.valid := preambleDetected & gfskRX.digital.out.valid
  gfskRX.digital.out.ready := io.digital.out.ready
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

  def RFtoIF(in: Seq[Double], Fc: Double): Seq[(Double, Double)] = {
    val timeSteps = Seq.tabulate[Double]((analog_F_sample * symbol_time * (in.length / 10)).toInt)(_ * (1/(analog_F_sample)))
    val frequencies = timeSteps.map {t => Fc + in((t / (symbol_time / 10)).floor.toInt) * 0.25 * MHz }
    val phases = frequencies.map{var s: Double = 0.0; d => {s += d; 2 * math.Pi * s * (1/analog_F_sample)}}
    val rf = phases.map {math.cos(_)}
    val I = timeSteps.indices.map {i => rf(i) * math.cos(2 * math.Pi * F_LO * timeSteps(i))}
    val Q = timeSteps.indices.map {i => rf(i) * math.sin(2 * math.Pi * F_LO * timeSteps(i))}
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
    analogToDigital(RFtoIF(FIR(bitstream(bits), gaussian_weights), centerFrequency))
  }
  it should "PASS Fuzz" in {
    test(new GFSKRXTestModule(new BLEBasebandModemParams())).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val inDriverI = new DecoupledDriverMaster(c.clock, c.io.analog.i)
      val inDriverQ = new DecoupledDriverMaster(c.clock, c.io.analog.q)
      val outDriver = new DecoupledDriverSlave(c.clock, c.io.digital.out)
      val outMonitor = new DecoupledMonitor(c.clock, c.io.digital.out)
      val numberOfBits = 100
      val preamble = Seq(1,0,1,0,1,0,1,0)
      val packet = Seq.tabulate(numberOfBits){_ => Random.nextInt(2)}
      val bits = Seq(0,0,0,0,0,0) ++ preamble ++ packet ++ Seq(0,0,0,0,0,0,0)
      val input = testWaveform(bits)
      inDriverI.push(input.map(p => new DecoupledTX(UInt(5.W)).tx(p._1.U(5.W))))
      inDriverQ.push(input.map(p => new DecoupledTX(UInt(5.W)).tx(p._2.U(5.W))))
      c.clock.step(bits.size * 20)
      println(outMonitor.monitoredTransactions.map{_.data.litValue})
      assert(packet.zip(outMonitor.monitoredTransactions.map{_.data.litValue}).forall {p => p._1 == p._2})
    }
  }
  it should "PASS Radio Frequency" in {
    test(new HilbertFilter(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.out.data.ready.poke(1.B)
      var arr = Seq[BigInt]()
      var i = 0
      val input = testWaveform(Seq(0,0,0,0,1,0,1,0,1,0,1,0,1,0,1,0))
      while (i < input.length) {
        c.io.in.i.bits.poke(input(i)._1.asUInt())
        c.io.in.q.bits.poke(input(i)._2.asUInt())
        c.io.in.q.valid.poke((i < input.length).asBool())
        c.io.in.i.valid.poke((i < input.length).asBool())
        c.clock.step()
       if (c.io.out.data.valid.peek().litToBoolean)
          arr = arr ++ Seq(c.io.out.data.bits.peek().litValue())
        i+=1
      }
      print("Output:\n")
      print(arr)
      assert(true)
    }
  }

  it should "REJECT IMAGE" in {
    test(new HilbertFilter(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.out.data.ready.poke(1.B)
      var arr = Seq[BigInt]()
      var i = 0
      val input = testWaveform(Seq(0,0,0,1,1,1,0,0,0,1,1,1,0,0,0), F_IM)
      while (i < input.length) {
        c.io.in.i.bits.poke(input(i)._1.asUInt())
        c.io.in.q.bits.poke(input(i)._2.asUInt())
        c.io.in.q.valid.poke((i < input.length).asBool())
        c.io.in.i.valid.poke((i < input.length).asBool())
        c.clock.step()
       if (c.io.out.data.valid.peek().litToBoolean)
          arr = arr ++ Seq(c.io.out.data.bits.peek().litValue())
        i+=1
      }
      print("Output:\n")
      print(arr)
      assert(true)
    }
  }
}