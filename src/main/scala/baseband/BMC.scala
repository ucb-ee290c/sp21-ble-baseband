package baseband

import chisel3._
import chisel3.util._
import modem.{GFSKModem, GFSKModemAnalogIO, GFSKModemLUTCommand, GFSKModemTuningControlIO}

import ee290cdma._

// Baseband, Modem, and Controller Paired in a Unit
class BMC(params: BLEBasebandModemParams, beatBytes: Int) extends Module {
  val io = IO(new Bundle {
    val analog = new Bundle {
      val data = new GFSKModemAnalogIO(params)
      val enable = new Bundle {
        val rx = Output(UInt(5.W))
      }
      val offChipMode = new Bundle {
        val rx = Output(Bool())
        val tx = Output(Bool())
      }
    }
    val cmd = Flipped(Decoupled(new BLEBasebandModemCommand))
    val  dma = new Bundle {
      val readReq = Decoupled(new EE290CDMAReaderReq(params.paddrBits, params.maxReadSize))
      val readResp = Flipped(Decoupled(new EE290CDMAReaderResp(params.maxReadSize)))
      val readData = Flipped(Decoupled(UInt((beatBytes * 8).W)))
      val writeReq = Decoupled(new EE290CDMAWriterReq(params.paddrBits, beatBytes))
    }
    val interrupt = Output(new BLEBasebandModemInterrupts)
    val lutCmd = Flipped(Decoupled(new GFSKModemLUTCommand))
    val messages =  new BLEBasebandModemMessagesIO
    val tuning = new Bundle {
      val data = new Bundle {
        val i = new Bundle {
          val vgaAtten = Output(UInt(10.W))
        }
        val q = new Bundle {
          val vgaAtten = Output(UInt(10.W))
        }
        val dac = new Bundle {
          val t0 = Output(UInt(6.W))
          val t2 = Output(UInt(6.W))
        }
      }
      val control = Input(new GFSKModemTuningControlIO(params))
    }
    val state = new Bundle {
      // State 0 components
      val assemblerState = Output(UInt(log2Ceil(6+1).W)) // 3
      val disassemblerState = Output(UInt(log2Ceil(7+1).W)) // 3
      val txState = Output(UInt(log2Ceil(2+1).W)) // 2
      val rxControllerState = Output(UInt(log2Ceil(4+1).W)) // 3
      val txControllerState = Output(UInt(log2Ceil(3+1).W)) // 2
      val mainControllerState = Output(UInt(log2Ceil(4+1).W)) // 3
      val imageRejectionOut = Output(SInt((params.adcBits + 3).W))  // 11
      // TOTAL: 27

      // State 1 components
      val preambleDetected = Output(Bool()) // 1
      val bandpassF0 = Output(UInt((params.adcBits + 3 + 12).W)) // 23
      // + ADC I 8
      // TOTAL: 32

      // State 2 components
      val bandpassF1 = Output(UInt((params.adcBits + 3 + 12).W)) // 23
      val accumulatorCount = Output(SInt(log2Ceil((params.samplesPerSymbol * 2) + 1).W)) // 7
      // TOTAL: 30

      // State 3 Components
      val envelopeF0 = Output(UInt((params.adcBits + 3 + 1).W)) // 12
      val envelopeF1 = Output(UInt((params.adcBits + 3 + 1).W)) // 12
      // + ADC Q 8
      // TOTAL: 32

      // State 4 Components
      val gfskIndex = Output(UInt(6.W))  // 6
      val i = new Bundle {
        val agcIndex = Output(UInt(5.W)) // 5
        val dcoIndex = Output(UInt(5.W)) // 5
      }
      val q = new Bundle {
        val agcIndex = Output(UInt(5.W)) // 5
        val dcoIndex = Output(UInt(5.W)) // 5
      }
      // TOTAL: 26
    }
  })

  // Controller
  val controller = Module(new Controller(params, beatBytes))
  controller.io.cmd <> io.cmd
  controller.io.dma.readReq <> io.dma.readReq
  controller.io.dma.readResp <> io.dma.readResp

  // Baseband
  val baseband = Module(new Baseband(params, beatBytes))
  baseband.io.control <> controller.io.basebandControl
  baseband.io.constants := controller.io.constants
  baseband.io.dma.readData <> io.dma.readData
  baseband.io.dma.writeReq <> io.dma.writeReq

  // Modem
  val modem = Module(new GFSKModem(params))
  modem.io.lutCmd <> io.lutCmd
  modem.io.analog.rx <> io.analog.data.rx
  modem.io.constants := controller.io.constants
  modem.io.control <> controller.io.modemControl
  modem.io.digital.tx <> baseband.io.modem.digital.tx
  modem.io.digital.rx <> baseband.io.modem.digital.rx
  modem.io.tuning <> io.tuning

  baseband.io.modem.control.preambleDetected := modem.io.control.rx.out.preambleDetected
  modem.io.control.rx.in.imageRejectionControl := io.tuning.control.imageRejectionControl
  modem.io.control.rx.in.enable := controller.io.analog.enable.rx
  modem.io.control.rx.in.accessAddressLSB := controller.io.constants.accessAddress(0)
  modem.io.control.rx.in.preambleDetectionThreshold := io.tuning.control.preambleDetectionThreshold

  // State
  io.state.assemblerState := baseband.io.state.assemblerState
  io.state.disassemblerState := baseband.io.state.disassemblerState
  io.state.txState := modem.io.state.txState
  io.state.rxControllerState := controller.io.state.rxControllerState
  io.state.txControllerState := controller.io.state.txControllerState
  io.state.mainControllerState := controller.io.state.mainControllerState
  io.state.preambleDetected := modem.io.control.rx.out.preambleDetected
  io.state.imageRejectionOut := modem.io.state.imageRejectionOut
  io.state.bandpassF0 := modem.io.state.bandpassF0
  io.state.bandpassF1 := modem.io.state.bandpassF1
  io.state.envelopeF0 := modem.io.state.envelopeF0
  io.state.envelopeF1 := modem.io.state.envelopeF1
  io.state.accumulatorCount := modem.io.state.accumulatorCount
  io.state.i.agcIndex := modem.io.state.i.agcIndex
  io.state.i.dcoIndex := modem.io.state.i.dcoIndex
  io.state.q.agcIndex := modem.io.state.q.agcIndex
  io.state.q.dcoIndex := modem.io.state.q.dcoIndex
  io.state.gfskIndex := modem.io.state.gfskIndex

  // Interrupts
  io.messages <> controller.io.messages
  io.interrupt.rxError := controller.io.interrupt.rxError
  io.interrupt.rxStart := controller.io.interrupt.rxStart
  io.interrupt.rxFinish := controller.io.interrupt.rxFinish
  io.interrupt.txError := controller.io.interrupt.txError
  io.interrupt.txFinish := controller.io.interrupt.txFinish

  // Enables
  io.analog.enable.rx := controller.io.analog.enable.rx

  io.analog.data.pllD := controller.io.analog.pllD
  io.analog.data.loCT := modem.io.analog.loCT
  io.analog.data.tx.loFSK := modem.io.analog.tx.loFSK

  io.analog.offChipMode := controller.io.analog.offChipMode
}