package baseband

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, VerilatorBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

import verif._
import  ee290cdma._

class ControllerTest extends AnyFlatSpec with ChiselScalatestTester {
  val tests = 40

  def seqToBinary(in: Seq[Int]): Seq[Int] = {
    in.map(x => String.format("%8s", x.toBinaryString).replaceAll(" ", "0").reverse).mkString("").map(c => c.toString.toInt)
  }

  def seqToWidePackets(beatBytes: Int, seq: Seq[Int]): (Seq[BigInt], Seq[Int]) = {
    var in = seq
    var out = Seq[BigInt]()
    var lengths = Seq[Int]()

    while (in.nonEmpty) {
      val (group, rest) = in.splitAt(beatBytes)
      val bytes = group.padTo(beatBytes, 0)

      var sum = BigInt(0)
      for (i <- 0 until beatBytes) {
        sum = sum + (BigInt(bytes(i)) << (8*i))
      }
      lengths = lengths :+ group.length
      out = out :+ sum
      in = rest
    }
    (out, lengths)
  }

  it should "Send correct signals" in {
    val beatBytes = 4
    val params = BLEBasebandModemParams()
    test(new TXChainController(params)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val dmaReadReqDriver = new DecoupledDriverSlave(c.clock, c.io.dma.readReq, 0)
      val dmaReadReqMonitor = new DecoupledMonitor(c.clock, c.io.dma.readReq)
      val assemblerInControlDriver = new DecoupledDriverSlave(c.clock, c.io.assemblerControl.in, 0)
      val assemblerInControlMonitor = new DecoupledMonitor(c.clock, c.io.assemblerControl.in)
      val controllerCmdInDriver = new DecoupledDriverMaster(c.clock, c.io.control.cmd)

      controllerCmdInDriver.push(new DecoupledTX(new TXChainControllerCommand(params.paddrBits, params.maxReadSize)).tx(
        new TXChainControllerCommand(params.paddrBits, params.maxReadSize).Lit(_.totalBytes -> 4.U, _.addr -> "x4000".U)
      ))

      c.clock.step(10)
      println(assemblerInControlMonitor.monitoredTransactions.map(tx => tx.data))
      println(dmaReadReqMonitor.monitoredTransactions.map(tx => tx.data))
    }
  }

  it should "elaborate Controller" in {
    val params = BLEBasebandModemParams()
    test(new Controller(params)).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.clock.step(10)
    }
  }
}