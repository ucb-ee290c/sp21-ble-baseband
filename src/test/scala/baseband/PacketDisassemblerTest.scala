package baseband

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

import verif._

class PacketDisassemblerTest extends AnyFlatSpec with ChiselScalatestTester {
  def seqToBinary(in: Seq[Int]): Seq[Int] = {
    in.map(x => String.format("%8s", x.toBinaryString).replaceAll(" ", "0").reverse).mkString("").map(c => c.toString.toInt)
  }

  it should "Convert binary to bytes with no whitening" in {
    test(new PacketDisassembler).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val controlDriver = new DecoupledDriverMaster(c.clock, c.io.in.control)
      val inDriver = new DecoupledDriverMaster(c.clock, c.io.in.data)
      val dmaDriver = new DecoupledDriverSlave(c.clock, c.io.out.data, 0)
      val dmaMonitor = new DecoupledMonitor(c.clock, c.io.out.data)

      val pduLength = scala.util.Random.nextInt(255)
      val inBytes = Seq(0, pduLength) ++ Seq.tabulate(pduLength)(_ => scala.util.Random.nextInt(255))
      val aa = BigInt("8E89BED6", 16)

      val preambleBits = "01010101".map(c => c.toString.toInt)
      val aaBits = aa.toInt.toBinaryString.reverse.map(c => c.toString.toInt)
      val badAABits = BigInt("8E87BDD5", 16).toInt.toBinaryString.reverse.map(c => c.toString.toInt)
      val inBits = preambleBits ++ aaBits ++ seqToBinary(inBytes)

      c.io.constants.channelIndex.poke("b000000".U)

      inDriver.push(inBits.map(x => (new DecoupledTX(UInt(1.W))).tx(x.U)))
      controlDriver.push(new DecoupledTX(new PDAControlInputBundle).tx((new PDAControlInputBundle).Lit(_.aa -> aa.U)))

      c.clock.step(400)

      val expectedOut = inBytes

      // Check all things match with no whitening
      dmaMonitor.monitoredTransactions
        .map(x => x.data.litValue())
        .zip(expectedOut)
        .foreach {case (o, e) => assert(o == e)}
    }
  }

  it should "Fail to convert binary to bytes with whitening" in {
    test(new PacketDisassembler).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val controlDriver = new DecoupledDriverMaster(c.clock, c.io.in.control)
      val inDriver = new DecoupledDriverMaster(c.clock, c.io.in.data)
      val dmaDriver = new DecoupledDriverSlave(c.clock, c.io.out.data, 0)
      val dmaMonitor = new DecoupledMonitor(c.clock, c.io.out.data)

      val pduLength = scala.util.Random.nextInt(255)
      val inBytes = Seq(0, pduLength) ++ Seq.tabulate(pduLength)(_ => scala.util.Random.nextInt(255))
      val aa = BigInt("8E89BED6", 16)

      val preambleBits = "01010101".map(c => c.toString.toInt)
      val aaBits = aa.toInt.toBinaryString.reverse.map(c => c.toString.toInt)
      val badAABits = BigInt("8E87BDD5", 16).toInt.toBinaryString.reverse.map(c => c.toString.toInt)
      val inBits = preambleBits ++ aaBits ++ seqToBinary(inBytes)

      c.io.constants.channelIndex.poke((scala.util.Random.nextInt(62) + 1).U) // Poke random 6 bit value (not 0)

      inDriver.push(inBits.map(x => (new DecoupledTX(UInt(1.W))).tx(x.U)))
      controlDriver.push(new DecoupledTX(new PDAControlInputBundle).tx((new PDAControlInputBundle).Lit(_.aa -> aa.U)))

      c.clock.step(400)

      val expectedOut = inBytes

      // Check that there is some mismatch with whitening
      assert(dmaMonitor.monitoredTransactions
        .map(x => x.data.litValue())
        .zip(expectedOut)
        .map({case (o, e) => o == e})
        .reduce(_&&_) == false)
    }
  }
}