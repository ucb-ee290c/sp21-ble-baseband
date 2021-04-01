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

  val gausFirData = Seq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.0001227184630308513, -0.0002454369260617026, -0.000859029241215959, -0.0014726215563702154, -0.0033133985018329853, -0.005154175447295755, -0.0095720401164064, -0.013989904785517048, -0.023071071049800045, -0.03215223731408304, -0.048596511360217115, -0.06504078540635118, -0.0914252549579842, -0.11780972450961724, -0.15585244804918114, -0.19389517158874503, -0.24359614911623978, -0.2932971266437346, -0.3529382996767283, -0.412579472709722, -0.47958375352456684, -0.5465880343394116, -0.6182556167494287, -0.689923199159446, -0.764167869293111, -0.838412539426776, -0.9138843941907495, -0.9893562489547232, -1.0648281037186969, -1.1402999584826705, -1.215771813246644, -1.2912436680106176, -1.3667155227745913, -1.442187377538565, -1.5176592323025384, -1.593131087066512, -1.6686029418304857, -1.7440747965944594, -1.8195466513584329, -1.8950185061224065, -1.9702449239603184, -2.04547134179823, -2.1197160119318954, -2.1939606820655606, -2.2657509829386084, -2.3375412838116563, -2.4041774092374086, -2.470813534663161, -2.5281230568985684, -2.585432579133976, -2.6280158858056812, -2.6705991924773866, -2.693302108138094, -2.7160050237988016, -2.715391431483647, -2.7147778391684927, -2.6908477388774767, -2.666917638586461, -2.623107147284447, -2.579296655982433, -2.521005386042779, -2.4627141161031245, -2.396077990677372, -2.32944186525162, -2.260105933639189, -2.190770002026758, -2.1241338766010056, -2.0574977511752532, -2.000188228939846, -1.9428787067044382, -1.900295400032733, -1.8577120933610276, -1.8350091777003201, -1.8123062620396126, -1.812919854354767, -1.8135334466699216, -1.8374635469609373, -1.8613936472519532, -1.905204138553967, -1.949014629855981, -2.007551336721697, -2.0660880435874134, -2.133951353643474, -2.2018146636995346, -2.2748321492028913, -2.3478496347062476, -2.4233214894702213, -2.498793344234195, -2.574265198998168, -2.649737053762142, -2.7252089085261155, -2.800680763290089, -2.876152618054063, -2.9516244728180365, -3.02709632758201, -3.102568182345984, -3.178040037109957, -3.2535118918739303, -3.3289837466379035, -3.4044556014018768, -3.47992745616585, -3.555399310929823, -3.6308711656937964, -3.7063430204577696, -3.7818148752217433, -3.8572867299857165, -3.9327585847496898, -4.008230439513663, -4.083702294277636, -4.159174149041609, -4.234646003805583, -4.310117858569556, -4.3855897133335295, -4.461061568097502, -4.536533422861476, -4.612005277625449, -4.687477132389422, -4.762948987153395, -4.838420841917369, -4.9138926966813425, -4.989119114519254, -5.064345532357166, -5.138590202490831, -5.212834872624496, -5.2846251734975445, -5.356415474370593, -5.423051599796345, -5.4896877252220975, -5.546997247457505, -5.604306769692913, -5.646890076364619, -5.689473383036324, -5.712176298697032, -5.73487921435774, -5.734265622042585, -5.733652029727431, -5.709721929436415, -5.6857918291453995, -5.641981337843386, -5.598170846541372, -5.539634139675655, -5.48109743280994, -5.413234122753879, -5.345370812697819, -5.2723533271944625, -5.199335841691107, -5.123863986927133, -5.04839213216316, -4.9729202773991865, -4.897448422635213, -4.82197656787124, -4.746504713107266, -4.671032858343294, -4.59556100357932, -4.520089148815347, -4.4446172940513735, -4.369145439287401, -4.293673584523427, -4.218201729759453, -4.142729874995481, -4.067503457157569, -3.992277039319657, -3.9180323691859917, -3.8437876990523265, -3.7719973981792783, -3.70020709730623, -3.6335709718804776, -3.5669348464547252, -3.509625324219318, -3.45231580198391, -3.409732495312204, -3.3671491886404983, -3.344446272979791, -3.321743357319083, -3.322356949634237, -3.322970541949392, -3.3469006422404077, -3.3708307425314232, -3.414641233833437, -3.458451725135451, -3.5167429950751052, -3.57503426501476, -3.6416703904405123, -3.7083065158662643, -3.777642447478695, -3.846978379091126)

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
