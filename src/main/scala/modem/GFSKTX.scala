package modem

import chisel3._
import chisel3.util._
import chisel3.experimental.FixedPoint

class GFSKTX extends Module {
  val io = IO(new Bundle {
    val analog = new Bundle {
      val gfskIndex = Output(UInt(6.W))
    }
    val digital = new Bundle {
      val in = Flipped(Decoupled(UInt(1.W)))
    }
  })

  val cyclesPerSymbol = 20
  val cyclesPerSample = cyclesPerSymbol / 10 // Oversampling must be 10

  val gaussianFixedPointWeights = Seq(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.015625, 0.0625, 0.15625, 0.328125, 0.59375,
    0.9375, 1.265625, 1.484375, 1.484375, 1.265625, 0.9375, 0.59375, 0.328125, 0.15625, 0.0625, 0.015625)
    .map(w => FixedPoint.fromDouble(w, 8.W, 6.BP)) //removed last 7 coeffs: 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0

  val counter = RegInit(0.U(8.W))

  val firInValid = RegInit(false.B)
  val firInData = RegInit(0.F(2.W, 0.BP))

  val fir = Module(new GenericFIR(FixedPoint(2.W, 0.BP), FixedPoint(11.W, 6.BP), gaussianFixedPointWeights))
  fir.io.in.valid := firInValid
  fir.io.in.bits.data := firInData
  fir.io.out.ready := true.B // TOOD: Make this based on TX mode?

  io.digital.in.ready := counter === 0.U

  when(counter === 0.U) {
    when(io.digital.in.fire()) {
      firInValid := true.B
      firInData := Mux(io.digital.in.bits === 0.U, (-1).F(2.W, 0.BP), 1.F(2.W, 0.BP))
      counter := counter + 1.U
    }.otherwise {
      firInValid := false.B
    }
  }.elsewhen(counter =/= 0.U) {
    counter := Mux(counter === cyclesPerSymbol.U, 0.U, counter + 1.U)
    when(fir.io.in.fire()) {
      firInValid := false.B
    }.elsewhen(counter % cyclesPerSample.U === 0.U) {
      firInValid := true.B
    }
  }

  val firOut = fir.io.out.bits.data
  io.analog.gfskIndex := firOut(firOut.getWidth - 1, firOut.getWidth - 6)
}