package modem

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

import verif._
import baseband.BLEBasebandModemParams

class GFSKTXTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "Elaborate a GFSKTX" in {
    test(new GFSKTX).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val inDriver = new DecoupledDriverMaster(c.clock, c.io.digital.in)

      val inBits = Seq(1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0)

      inDriver.push(inBits.map(b => new DecoupledTX(UInt(1.W)).tx(b.U)))

      val outCodes = scala.collection.mutable.Queue[Int]()

      while (outCodes.length != inBits.length * 20) {
        outCodes += c.io.analog.gfskIndex.peek().litValue().toInt
        c.clock.step()
      }

      val signedOut = outCodes.map(c => (c & 0x1F) - (c & 0x20))

      println(outCodes.length)
      println(signedOut)

      assert(true)
    }
  }
}