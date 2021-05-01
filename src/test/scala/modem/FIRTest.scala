package modem

import chisel3._
import chisel3.util._
import chisel3.experimental._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

import verif._

class FIRTest extends AnyFlatSpec with ChiselScalatestTester {
  val testWeights = Seq[Double](1, 2, 3).map(w => FixedPoint.fromDouble(w, 4.W, 0.BP))
  val testInputs = Seq.tabulate(10)(i => scala.util.Random.nextInt(4)).map(i => FixedPoint.fromDouble(i, 4.W, 0.BP))

  it should "Test A Transpose FIR" in {

    test(new FixedPointTransposeFIR(FixedPoint(4.W, 0.BP), FixedPoint(6.W, 0.BP), FixedPoint(4.W, 0.BP), testInputs.length)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.out.ready.poke(true.B)
      c.io.coeff.poke(VecInit(testInputs))
      c.clock.step()

      val inDriver = new DecoupledDriverMaster(c.clock, c.io.in)
      //val outDriver = new DecoupledDriverSlave(c.clock, c.io.out)
      //val outMonitor = new DecoupledMonitor(c.clock, c.io.out)

      inDriver.push(testInputs.map(input => (new DecoupledTX(FixedPoint(4.W, 0.BP))).tx(input)))

      c.clock.step(50)
    }
  }

  it should "Test A Direct FIR" in {
    val genIn = FixedPoint(4.W, 0.BP)
    val genOut = FixedPoint(6.W, 0.BP)
    val c = FixedPoint(4.W, 0.BP)
    test(new GenericFIR(genIn, genOut, testWeights)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.io.out.ready.poke(true.B)

      c.clock.step()

      val inDriver = new DecoupledDriverMaster(c.clock, c.io.in)
      //val outDriver = new DecoupledDriverSlave(c.clock, c.io.out)
      //val outMonitor = new DecoupledMonitor(c.clock, c.io.out)

      inDriver.push(testInputs.map(input => (new DecoupledTX(new GenericFIRBundle(genIn)))
        .tx(new GenericFIRBundle(genIn)
          .Lit(_.data -> input)
        )))

      c.clock.step(50)
    }
  }
}