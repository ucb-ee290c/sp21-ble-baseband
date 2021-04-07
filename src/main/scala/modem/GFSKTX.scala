package modem

import baseband.BLEBasebandModemParams
import chisel3._
import chisel3.util._
import chisel3.experimental.FixedPoint

class GFSKTXControlInputBundle(val params: BLEBasebandModemParams) extends Bundle {
  private val maxPacketSize = 1 + 4 + 2 + params.maxReadSize + 3

  val totalBytes = UInt(log2Ceil(maxPacketSize).W)
}

class GFSKTXControlOutputBundle extends Bundle {
  val done = Bool()
}

class GFSKTXControlIO(val params: BLEBasebandModemParams) extends Bundle {
  val in = Flipped(Decoupled(new GFSKTXControlInputBundle(params)))
  val out = Output(new GFSKTXControlOutputBundle)
}

class GFSKTX(params: BLEBasebandModemParams) extends Module {
  val io = IO(new Bundle {
    val analog = new Bundle {
      val gfskIndex = Output(UInt(6.W))
    }
    val digital = new Bundle {
      val in = Flipped(Decoupled(UInt(1.W)))
    }
    val control = new GFSKTXControlIO(params)
  })

  val maxPacketSize = 1 + 4 + 2 + params.maxReadSize + 3

  val s_idle :: s_working :: nil = Enum(2)

  val state = RegInit(s_idle)

  val cyclesPerSymbol = 20
  val cyclesPerSample = cyclesPerSymbol / 10 // Oversampling must be 10

  val counter = RegInit(0.U(8.W))
  val counterBytes = RegInit(0.U(3.W)) // Counts bits within a byte

  val sentBytes = RegInit(0.U(log2Ceil(maxPacketSize).U))
  val totalBytes = RegInit(0.U(log2Ceil(maxPacketSize).U))

  val done = RegInit(false.B)

  val firInValid = RegInit(false.B)
  val firInData = RegInit(0.F(2.W, 0.BP))

  val firWeights = FIRCoefficients.gaussianWeights.map(w => FixedPoint.fromDouble(w, 8.W, 6.BP))
  val fir = Module(new GenericFIR(FixedPoint(2.W, 0.BP), FixedPoint(11.W, 6.BP), firWeights))
  fir.io.in.valid := firInValid
  fir.io.in.bits.data := firInData
  fir.io.out.ready := true.B // TODO: Make this based on TX mode?

  io.digital.in.ready := counter === 0.U && state === s_working

  io.control.in.ready := state === s_idle
  io.control.out.done := done

  when(state === s_idle) {
    done := false.B
    when (io.control.in.fire()) {
      state := s_idle
      counter := 0.U
      totalBytes := io.control.in.bits.totalBytes + 10.U
      sentBytes := 0.U
    }
  }.elsewhen(state === s_working) {
    when(counter === 0.U) {
      when(io.digital.in.fire()) {
        firInValid := true.B
        firInData := Mux(io.digital.in.bits === 0.U, (-1).F(2.W, 0.BP), 1.F(2.W, 0.BP))
        counter := counter + 1.U
      }.otherwise {
        firInValid := false.B
      }
    }.elsewhen(counter =/= 0.U) {
      counter := Mux(counter === (cyclesPerSymbol - 1).U, 0.U, counter + 1.U)

      when(counter === (cyclesPerSymbol - 1).U) { // TODO: Verify this
        counterBytes := Mux(counterBytes === 7.U, 0.U, counterBytes + 1.U)

        when(counterBytes === 7.U) {
          when(sentBytes === (totalBytes - 1.U)) {
            sentBytes := 0.U
            done := true.B
            state := s_idle
          }.otherwise {
            sentBytes := sentBytes + 1.U
          }
        }
      }
      when(fir.io.in.fire()) {
        firInValid := false.B
      }.elsewhen(counter % cyclesPerSample.U === 0.U) {
        firInValid := true.B
      }
    }
  }

  val firOut = fir.io.out.bits.data
  io.analog.gfskIndex := firOut(firOut.getWidth - 1, firOut.getWidth - 6)
}