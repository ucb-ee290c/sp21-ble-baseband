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

class GFSKRXTestModule(params: BLEBasebandModemParams) extends Module {
  var io = IO(new Bundle {
    val in = new AnalogRXIO(params)
    val out = new Bundle {
      val f0 = SInt(8.W)
      val f1 = SInt(8.W)
    }
  })

  val hilbertFilter = Module(new HilbertFilter(params))
  hilbertFilter.io.in <> io.in

  val bandpassF0 = Module( new GenericFIR(FixedPoint(6.W, 0.BP), FixedPoint(19.W, 11.BP),
    FIRCoefficients.GFSKRX_Bandpass_F0.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))) )

  val bandpassF1 = Module( new GenericFIR(FixedPoint(6.W, 0.BP), FixedPoint(19.W, 11.BP),
    FIRCoefficients.GFSKRX_Bandpass_F1.map(c => FixedPoint.fromDouble(c, 12.W, 11.BP))) )

  bandpassF0.io.in.bits := hilbertFilter.io.out.data.bits(6, 1).asFixedPoint(0.BP)
  hilbertFilter.io.out.data.ready := bandpassF0.io.in.ready
  bandpassF0.io.in.valid := hilbertFilter.io.out.data.valid

  bandpassF1.io.in.bits := hilbertFilter.io.out.data.bits(6, 1).asFixedPoint(0.BP)
  hilbertFilter.io.out.data.ready := bandpassF1.io.in.ready
  bandpassF1.io.in.valid := hilbertFilter.io.out.data.valid

  bandpassF0.io.out.ready := 1.B
  io.out.f0 := bandpassF0.io.out.bits.data.asSInt()(18, 11)

  bandpassF1.io.out.ready := 1.B
  io.out.f1 := bandpassF1.io.out.bits.data.asSInt()(18, 11)

}

class GFSKRXTest extends AnyFlatSpec with ChiselScalatestTester {

  val gausFirData = Seq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0015625, 0.0015625, 0.0078125, 0.0078125, 0.0234375, 0.0234375, 0.05625, 0.05625, 0.115625, 0.115625, 0.209375, 0.209375, 0.3359375, 0.3359375, 0.484375, 0.484375, 0.6328125, 0.6328125, 0.759375, 0.759375, 0.853125, 0.853125, 0.9125, 0.9125, 0.9453125, 0.9453125, 0.9609375, 0.9609375, 0.9671875, 0.9671875, 0.96875, 0.96875, 0.96875, 0.96875, 0.96875, 0.96875, 0.96875, 0.96875, 0.96875, 0.96875, 0.965625, 0.965625, 0.953125, 0.953125, 0.921875, 0.921875, 0.85625, 0.85625, 0.7375, 0.7375, 0.55, 0.55, 0.296875, 0.296875, 0.0, 0.0, -0.296875, -0.296875, -0.55, -0.55, -0.7375, -0.7375, -0.85625, -0.85625, -0.921875, -0.921875, -0.953125, -0.953125, -0.965625, -0.965625, -0.96875, -0.96875, -0.96875, -0.96875, -0.96875, -0.96875, -0.96875, -0.96875, -0.96875, -0.96875, -0.965625, -0.965625, -0.953125, -0.953125, -0.921875, -0.921875, -0.85625, -0.85625, -0.7375, -0.7375, -0.55, -0.55, -0.296875, -0.296875, 0.0, 0.0, 0.296875, 0.296875, 0.55, 0.55, 0.734375, 0.734375, 0.840625, 0.840625, 0.875, 0.875, 0.840625, 0.840625, 0.734375, 0.734375, 0.55, 0.55, 0.296875, 0.296875, 0.0, 0.0, -0.296875, -0.296875, -0.55, -0.55, -0.734375, -0.734375, -0.840625, -0.840625, -0.875, -0.875, -0.840625, -0.840625, -0.734375, -0.734375, -0.55, -0.55, -0.296875, -0.296875, 0.0, 0.0, 0.296875, 0.296875, 0.55, 0.55, 0.7375, 0.7375, 0.85625, 0.85625, 0.921875, 0.921875, 0.953125, 0.953125, 0.965625, 0.965625, 0.96875, 0.96875, 0.96875, 0.96875, 0.96875, 0.96875, 0.96875, 0.96875, 0.96875, 0.96875, 0.965625, 0.965625, 0.953125, 0.953125, 0.921875, 0.921875, 0.85625, 0.85625, 0.7375, 0.7375, 0.55, 0.55, 0.296875, 0.296875, 0.0, 0.0, -0.296875, -0.296875, -0.55, -0.55, -0.7375, -0.7375, -0.85625, -0.85625, -0.921875, -0.921875)

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

  def RFtoIF(in: Seq[Double]): (Seq[Double], Seq[Double]) = {
    val timeSteps = Seq.tabulate[Double]((analog_F_sample * symbol_time * 10).toInt)(_ * (1/(analog_F_sample)))
    val rf = {t:Double => math.cos(2 * math.Pi * (F_RF + 0.25 * MHz) * t + math.Pi / 4)}
    val I = {t:Double => rf(t) * math.cos(2 * math.Pi * F_LO * t)}
    val Q = {t:Double => rf(t) * math.sin(2 * math.Pi * F_LO * t)}
    return (analogLowpass(timeSteps.map{I}, analog_F_sample, 10 * MHz).zipWithIndex.collect {case (e,i) if (i % (analog_F_sample / digital_clock_F).toInt) == 0 => e},
      analogLowpass(timeSteps.map{Q}, analog_F_sample, 10 * MHz).zipWithIndex.collect {case (e,i) if (i % (analog_F_sample / digital_clock_F).toInt) == 0 => e})
  }

  it should "Elaborate a modem" in {
    test(new GFSKRXTestModule(new BLEBasebandModemParams())).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      var out = List()
      val zipp = RFtoIF(Seq()).zipped
      val values = zipp.map{(i: Double, q:Double) => ((i - zipp.map{(i: Double, q:Double) => i}.min) / ((i - zipp.map{(i: Double, q:Double) => i}.max) - (i - zipp.map{(i: Double, q:Double) => i}.min)) * 31, (q - zipp.map{(i: Double, q:Double) => q}.min) / ((q - zipp.map{(i: Double, q:Double) => q}.max) - (q - zipp.map{(i: Double, q:Double) => q}.min)) * 31)}
      for (idx <- 0 to values.size) {
        val i = values(idx)._1
        val q = values(idx)._2
        c.io.in.i.data.poke(i.toInt.U)
        c.io.in.q.data.poke(q.toInt.U)
        c.io.in.i.valid.poke(1.B)
        c.io.in.q.valid.poke(1.B)
        c.clock.step()
        out ++= List(c.io.out.f1.peek().litValue())
      }
      print(out)
    }
  }
}
