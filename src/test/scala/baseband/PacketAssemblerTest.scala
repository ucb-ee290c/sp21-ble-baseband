package baseband

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

import verif._

class PacketAssemblerTest extends AnyFlatSpec with ChiselScalatestTester {
  def seqToBinary(in: Seq[Int]): Seq[Int] = {
    in.map(x => String.format("%8s", x.toBinaryString).replaceAll(" ", "0").reverse).mkString("").map(c => c.toString.toInt)
  }

  it should "Match binary output to input bytes with no whitening" in {
    test(new PacketAssembler).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val controlDriver = new DecoupledDriverMaster(c.clock, c.io.in.control)
      val dmaDriver = new DecoupledDriverMaster(c.clock, c.io.in.data)
      val outDriver = new DecoupledDriverSlave(c.clock, c.io.out.data, 0)
      val outMonitor = new DecoupledMonitor(c.clock, c.io.out.data)

      val pduLength = 15
      val inBytes = Seq.tabulate(pduLength + 2)(i => i)
      val aa = BigInt("8E89BED6", 16)

      c.io.constants.whiteningSeed.poke("b0000000".U)

      val preambleExpected = "01010101".map(c => c.toString.toInt)
      val aaExpected = aa.toInt.toBinaryString.reverse.map(c => c.toString.toInt)

      dmaDriver.push(inBytes.map(x => (new DecoupledTX(UInt(8.W))).tx(x.U)))
      controlDriver.push(new DecoupledTX(new PAControlInputBundle).tx((new PAControlInputBundle).Lit(_.aa -> aa.U, _.pduLength -> pduLength.U)))

      c.clock.step(400)

      // Check all things match with no whitening
      val expectedOut = preambleExpected ++ aaExpected ++ seqToBinary(inBytes)
      outMonitor.monitoredTransactions
        .map(x => x.data.litValue())
        .zip(expectedOut)
        .foreach {case (o, e) => assert(o == e)}
    }
  }

  it should "Not match binary output to input bytes with whitening" in {
    test(new PacketAssembler).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val controlDriver = new DecoupledDriverMaster(c.clock, c.io.in.control)
      val dmaDriver = new DecoupledDriverMaster(c.clock, c.io.in.data)
      val outDriver = new DecoupledDriverSlave(c.clock, c.io.out.data, 0)
      val outMonitor = new DecoupledMonitor(c.clock, c.io.out.data)

      val pduLength = 15
      val inBytes = Seq.tabulate(pduLength + 2)(i => i) // Need to add two for the header <IMPORTANT: In controller make sure to request bytesRead-2 as pduLength>
      val aa = BigInt("8E89BED6", 16)

      c.io.constants.whiteningSeed.poke((scala.util.Random.nextInt(126) + 1).U) // Poke random 7 bit value (not 0)

      val preambleExpected = "01010101".map(c => c.toString.toInt)
      val aaExpected = aa.toInt.toBinaryString.reverse.map(c => c.toString.toInt)

      dmaDriver.push(inBytes.map(x => (new DecoupledTX(UInt(8.W))).tx(x.U)))
      controlDriver.push(new DecoupledTX(new PAControlInputBundle).tx((new PAControlInputBundle).Lit(_.aa -> aa.U, _.pduLength -> pduLength.U)))

      c.clock.step(400)

      val expectedOut = preambleExpected ++ aaExpected ++ seqToBinary(inBytes)

      // Check Preamble and AA match
      assert(outMonitor.monitoredTransactions
        .map(x => x.data.litValue())
        .zip(expectedOut)
        .take(40)
        .map({case (o, e) => o == e})
        .reduce(_&&_))

      // Check there is some mismatch in PDU
      assert(outMonitor.monitoredTransactions
        .map(x => x.data.litValue())
        .zip(expectedOut)
        .drop(40) // Drop preamble and AA
        .map({case (o, e) => o == e})
        .reduce(_&&_) == false)
    }
  }
}