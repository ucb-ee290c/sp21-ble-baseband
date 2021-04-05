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

import verif._
import scala.collection.mutable.ListBuffer
/*
class GFSKRXTestModule(params: BLEBasebandModemParams) extends Module {
  var io = IO(new Bundle {
    val in = Input(new Bundle() {
      val i = UInt(5.W)
      val q = UInt(5.W)
    })
    val out = Output(new Bundle {
      val f0 = SInt(8.W)
      val f1 = SInt(8.W)
    })
  })

  val hilbertFilter = Module(new HilbertFilter(params))
  hilbertFilter.io.in.i.data := io.in.i
  hilbertFilter.io.in.i.valid := 1.B
  hilbertFilter.io.in.q.data := io.in.i
  hilbertFilter.io.in.q.valid := 1.B

  val bandpassF0 = Module( new GenericFIR(FixedPoint(6.W, 0.BP), FixedPoint(19.W, 11.BP),
    FIRCoefficients.GFSKRX_Bandpass_F0.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))) )

  val bandpassF1 = Module( new GenericFIR(FixedPoint(6.W, 0.BP), FixedPoint(19.W, 11.BP),
    FIRCoefficients.GFSKRX_Bandpass_F1.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))) )

  bandpassF0.io.in.bits.data := hilbertFilter.io.out.data.bits(6, 1).asFixedPoint(0.BP)
  hilbertFilter.io.out.data.ready := bandpassF0.io.in.ready
  bandpassF0.io.in.valid := hilbertFilter.io.out.data.valid

  bandpassF1.io.in.bits.data := hilbertFilter.io.out.data.bits(6, 1).asFixedPoint(0.BP)
  hilbertFilter.io.out.data.ready := bandpassF1.io.in.ready
  bandpassF1.io.in.valid := hilbertFilter.io.out.data.valid

  bandpassF0.io.out.ready := 1.B
  io.out.f0 := bandpassF0.io.out.bits.data(18, 11).asSInt

  bandpassF1.io.out.ready := 1.B
  io.out.f1 := bandpassF1.io.out.bits.data(18, 11).asSInt

}
*/
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


  val gaussian_weights = Seq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.015625, 0.0625, 0.15625, 0.328125, 0.59375,0.9375, 1.265625, 1.484375, 1.484375, 1.265625, 0.9375, 0.59375, 0.328125, 0.15625, 0.0625, 0.015625, 0.0, 0.0,0.0, 0.0, 0.0, 0.0, 0.0)
  def bitstream(in: Seq[Int]): Seq[Double] = {
    in.flatMap{b: Int => Seq.tabulate(20){i: Int => (if (b == 0) -1.0 else 1.0)}}
  }

  def FIR(in: Seq[Double], coeff: Seq[Double]): Seq[Double] = {
    var samples = ListBuffer[Double]()
    return in.map { s: Double =>
      samples.prepend(s)
      samples.zip(coeff).map {g: (Double, Double) => g._1 * g._2}.sum / 10
    }
  }

  def RFtoIF(in: Seq[Double], Fc: Double): Seq[(Double, Double)] = {
    val timeSteps = Seq.tabulate[Double]((analog_F_sample * symbol_time * (in.length)).toInt)(_ * (1/(analog_F_sample)))

    val rf = {t:Double => 
	math.cos(2 * math.Pi * (Fc + (if (in((t / (symbol_time)).floor.toInt) == 1) 1 else -1) * 0.25 * MHz) * t)}
    val I = {t:Double => rf(t) * math.cos(2 * math.Pi * F_LO * t)}
    val Q = {t:Double => rf(t) * math.sin(2 * math.Pi * F_LO * t)}
    return analogLowpass(timeSteps.map{I}, analog_F_sample, 10 * MHz) zip analogLowpass(timeSteps.map{Q}, analog_F_sample, 10 * MHz)
  }
  def analogToDigital(in: (Seq[(Double, Double)])): (Seq[(Int, Int)]) = {
    val sampled = in.zipWithIndex.collect {case (e,i) if (i % (analog_F_sample / digital_clock_F).toInt) == 0 => e}
    val maxI = sampled.map{_._1}.max
    val maxQ = sampled.map{_._2}.max
    val minI = sampled.map{_._1}.min
    val minQ = sampled.map{_._2}.min
    return sampled.map{s: (Double, Double) => (((s._1 - minI) / (maxI - minI) * 31).toInt, ((s._2 - minQ) / (maxQ - minQ) * 31).toInt)}
  }

  it should "Elaborate a modem" in {
    test(new GFSKRX(new BLEBasebandModemParams())).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val inDriverI = new DecoupledDriverMaster(c.clock, c.io.analog.i)
      val inDriverQ = new DecoupledDriverMaster(c.clock, c.io.analog.q)
      val outDriver = new DecoupledDriverSlave(c.clock, c.io.digital.out)
      val outMonitor = new DecoupledMonitor(c.clock, c.io.digital.out)
      val input = analogToDigital(RFtoIF(Seq(1,0,1,0,1,0,1,0,1,0), F_RF))
      inDriverI.push(input.map(p => new DecoupledTX(UInt(5.W)).tx(p._1.U(5.W))))
      inDriverQ.push(input.map(p => new DecoupledTX(UInt(5.W)).tx(p._2.U(5.W))))
      c.clock.step(1000)
      println(outMonitor.monitoredTransactions.map{_.data})
      println(input)
    }
  }

  it should "gaussian FIR" in {
    test(new GFSKRX(new BLEBasebandModemParams())).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      println(bitstream(Seq(1,0,1,0,1,0,1,0,1,0)))
      println(FIR(bitstream(Seq(1,0,1,0,1,0,1,0,1,0)), gaussian_weights))
    }
  }

  it should "PASS Radio Frequency" in {
    test(new HilbertFilter(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.out.data.ready.poke(1.B)
      var arr = Seq[BigInt]()
      var i = 0
      val input = analogToDigital(RFtoIF(Seq(1,0,1,0,1,0,1,0,1,0), F_RF))
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
      val input = analogToDigital(RFtoIF(FIR(bitstream(Seq(1,0,1,0,1,0,1,0,1,0)), gaussian_weights), F_IM))
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
