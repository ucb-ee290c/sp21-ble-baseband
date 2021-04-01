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

  val gausFirData = Seq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.0001227184630308513, -0.0002454369260617026, -0.000859029241215959, -0.0014726215563702154, -0.0033133985018329853, -0.005154175447295755, -0.0095720401164064, -0.013989904785517048, -0.023071071049800045, -0.03215223731408304, -0.048596511360217115, -0.06504078540635118, -0.0914252549579842, -0.11780972450961724, -0.15585244804918114, -0.19389517158874503, -0.24359614911623978, -0.2932971266437346, -0.3529382996767283, -0.412579472709722, -0.4793383165985051, -0.5460971604872882, -0.6165375582669969, -0.6869779560467055, -0.757541072289445, -0.8281041885321844, -0.8947403139579367, -0.9613764393836891, -1.0186859616190966, -1.0759954838545043, -1.1185787905262097, -1.161162097197915, -1.1838650128586223, -1.2065679285193298, -1.2059543362041756, -1.2053407438890213, -1.1814106435980054, -1.1574805433069892, -1.1136700520049756, -1.0698595607029617, -1.0115682907633072, -0.953277020823653, -0.8866408953979006, -0.8200047699721482, -0.7506688383597173, -0.6813329067472863, -0.614696781321534, -0.5480606558957818, -0.49075113366037426, -0.4334416114249667, -0.3908583047532613, -0.34827499808155593, -0.3255720824208484, -0.3028691667601409, -0.30348275907529515, -0.30409635139044944, -0.32802645168146544, -0.35195655197248144, -0.39576704327449536, -0.4395775345765093, -0.4978688045161636, -0.5561600744558179, -0.6227961998815702, -0.6894323253073225, -0.7587682569197535, -0.8281041885321844, -0.8947403139579367, -0.9613764393836891, -1.0186859616190966, -1.0759954838545043, -1.1185787905262097, -1.161162097197915, -1.1838650128586223, -1.2065679285193298, -1.2059543362041756, -1.2053407438890213, -1.1814106435980054, -1.1574805433069892, -1.1136700520049756, -1.0698595607029617, -1.0115682907633072, -0.953277020823653, -0.8866408953979006, -0.8200047699721482, -0.7506688383597173, -0.6813329067472863, -0.614696781321534, -0.5480606558957818, -0.49075113366037426, -0.4334416114249667, -0.3908583047532613, -0.34827499808155593, -0.3255720824208484, -0.3028691667601409, -0.30348275907529515, -0.30409635139044944, -0.32802645168146544, -0.35195655197248144, -0.39576704327449536, -0.4395775345765093, -0.4978688045161636, -0.5561600744558179, -0.6227961998815702, -0.6894323253073225, -0.7587682569197535, -0.8281041885321844, -0.8947403139579367, -0.9613764393836891, -1.0186859616190966, -1.0759954838545043, -1.1185787905262097, -1.161162097197915, -1.1838650128586223, -1.2065679285193298, -1.2059543362041756, -1.2053407438890213, -1.1814106435980054, -1.1574805433069892, -1.1136700520049756, -1.0698595607029617, -1.0115682907633072, -0.953277020823653, -0.8866408953979006, -0.8200047699721482, -0.7506688383597173, -0.6813329067472863, -0.614696781321534, -0.5480606558957818, -0.49075113366037426, -0.4334416114249667, -0.3908583047532613, -0.34827499808155593, -0.3255720824208484, -0.3028691667601409, -0.30348275907529515, -0.30409635139044944, -0.32802645168146544, -0.35195655197248144, -0.39576704327449536, -0.4395775345765093, -0.4978688045161636, -0.5561600744558179, -0.6227961998815702, -0.6894323253073225, -0.7587682569197535, -0.8281041885321844, -0.8947403139579367, -0.9613764393836891, -1.0186859616190966, -1.0759954838545043, -1.1185787905262097, -1.161162097197915, -1.1838650128586223, -1.2065679285193298, -1.2059543362041756, -1.2053407438890213, -1.1814106435980054, -1.1574805433069892, -1.1136700520049756, -1.0698595607029617, -1.0115682907633072, -0.953277020823653, -0.8866408953979006, -0.8200047699721482, -0.7506688383597173, -0.6813329067472863, -0.614696781321534, -0.5480606558957818, -0.49075113366037426, -0.4334416114249667, -0.3908583047532613, -0.34827499808155593, -0.3255720824208484, -0.3028691667601409, -0.30348275907529515, -0.30409635139044944, -0.32802645168146544, -0.35195655197248144, -0.39576704327449536, -0.4395775345765093, -0.4978688045161636, -0.5561600744558179, -0.6227961998815702, -0.6894323253073225, -0.7587682569197535, -0.8281041885321844)

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

  def RFtoIF(in: Seq[Double]): Seq[(Double, Double)] = {
    val timeSteps = Seq.tabulate[Double]((analog_F_sample * symbol_time * 10).toInt)(_ * (1/(analog_F_sample)))
    val rf = {t:Double => math.cos(2 * math.Pi * (F_RF) * t + math.Pi / 4 + gausFirData((t / 1e-7).floor.toInt))}
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
      val input = analogToDigital(RFtoIF(Seq()))
      inDriverI.push(input.map(p => new DecoupledTX(UInt(5.W)).tx(p._1.U(5.W))))
      inDriverQ.push(input.map(p => new DecoupledTX(UInt(5.W)).tx(p._2.U(5.W))))
      c.clock.step(1000)
      println(outMonitor.monitoredTransactions)
    }
  }

  it should "Image Reject" in {
    test(new GFSKRX(new BLEBasebandModemParams)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val input = analogToDigital(RFtoIF(Seq()))
      println(input.map{_._1})
      println(input.map{_._2})
    }
  }
}
