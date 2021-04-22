package modem

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import breeze.plot.{Figure, plot}
import collection.immutable.Seq
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{TreadleBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec
import baseband.BLEBasebandModemParams
import chisel3.experimental.FixedPoint
import modem.TestUtility._
import net.sparja.syto.filter.{TransferFunctionBuilder, filterForward}

import scala.collection.mutable.ListBuffer
import scala.util.Random
import breeze.plot._

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
    val control = new Bundle {
      val aaLSB = Input(UInt(1.W))
    }
  })
  val gfskRX = Module(new GFSKRX(params)).io

  val preambleDetected = gfskRX.control.out.preambleDetected
  gfskRX.control.in.imageRejectionOp := 0.B
  gfskRX.control.in.enable := 1.B
  gfskRX.control.in.accessAddressLSB := io.control.aaLSB
  gfskRX.control.in.preambleDetectionThreshold := 140.U
  gfskRX.analog <> io.analog
  io.digital.out.bits := gfskRX.digital.out.bits
  io.digital.out.valid := preambleDetected & gfskRX.digital.out.valid
  gfskRX.digital.out.ready := io.digital.out.ready
}

class GFSKRXTest extends AnyFlatSpec with ChiselScalatestTester {

  it should "Determine SNR vs BER" in {
    val numberOfBytes = 256

    val accessAddress = scala.util.Random.nextInt.abs
    val packet = TestUtility.packet(accessAddress, numberOfBytes)._1
    val bits = Seq(0,0,0,0,0,0) ++ packet ++ Seq(0,0,0,0,0,0,0)

    val cleanSignal = fastIF(FIR(bitstream(bits), gaussian_weights))

    //val initialPhaseOffset = Random.nextInt(20)

    val SNRvBER = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 50, 100).map { SNR =>
      var BER = 2.0
      println(s"Testing SNR of $SNR:")

      test(new GFSKRXTestModule(BLEBasebandModemParams())).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
        val inDriverI = new DecoupledDriverMaster(c.clock, c.io.analog.i)
        val inDriverQ = new DecoupledDriverMaster(c.clock, c.io.analog.q)
        val outDriver = new DecoupledDriverSlave(c.clock, c.io.digital.out)
        val outMonitor = new DecoupledMonitor(c.clock, c.io.digital.out)

        val input = addNoiseToCleanSignal(cleanSignal, SNR)

        c.io.control.aaLSB.poke((accessAddress & 0x1).U)

        //c.clock.step(initialPhaseOffset) // random phase offset

        inDriverI.push(input.map(p => new DecoupledTX(UInt(8.W)).tx(p._1.U(8.W))))
        inDriverQ.push(input.map(p => new DecoupledTX(UInt(8.W)).tx(p._2.U(8.W))))
        c.clock.step(bits.size * 20)

        val retrieved = outMonitor.monitoredTransactions.map{_.data.litValue.toInt}
        val expected = packet.slice(8, packet.size)

        println(retrieved.length)
        println(expected.length)

        if (retrieved.length > 0) {
          BER = 1.0 - ((expected.zip(retrieved).count { case (observed, expected) => observed == expected }).toDouble / expected.length)
        }

        println((SNR, BER))
      }

      (SNR, BER)
    }

    println(SNRvBER)
  }

  it should "PASS Fuzz" in {
    test(new GFSKRXTestModule(new BLEBasebandModemParams())).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      val inDriverI = new DecoupledDriverMaster(c.clock, c.io.analog.i)
      val inDriverQ = new DecoupledDriverMaster(c.clock, c.io.analog.q)
      val outDriver = new DecoupledDriverSlave(c.clock, c.io.digital.out)
      val outMonitor = new DecoupledMonitor(c.clock, c.io.digital.out)
      val accessAddress = scala.util.Random.nextInt.abs

      val numberOfBytes = 256
      val packet = TestUtility.packet(accessAddress, numberOfBytes)._1
      val bits = Seq(0,0,0,0,0,0) ++ packet ++ Seq(0,0,0,0,0,0,0)
      val input = TestUtility.testWaveform(bits)
      val initialPhaseOffset = Random.nextInt(20)
      c.io.control.aaLSB.poke((accessAddress & 0x1).U)
      c.clock.step(initialPhaseOffset) // random phase offset
      inDriverI.push(input.map(p => new DecoupledTX(UInt(8.W)).tx(p._1.U(8.W))))
      inDriverQ.push(input.map(p => new DecoupledTX(UInt(8.W)).tx(p._2.U(8.W))))
      c.clock.step(bits.size * 20)

      val retrieved = outMonitor.monitoredTransactions.map{_.data.litValue.toInt}
      val expected = packet.slice(8, packet.size)
      println("Initial Phase Offset: ",initialPhaseOffset, "Received Data: ", retrieved, "Expected Data: ", expected)
      assert(retrieved.size > 0 && expected.zip(retrieved).forall {p => p._1 == p._2})
    }
  }
}
