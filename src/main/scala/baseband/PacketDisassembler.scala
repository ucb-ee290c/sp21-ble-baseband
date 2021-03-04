/* Based on (with permission) the Fall 2018 290C BLE Baseband
   https://github.com/ucberkeley-ee290c/fa18-ble/
 */
package baseband

import chisel3._
import chisel3.util._

class PDAControlInputBundle extends Bundle {
  val aa = UInt(32.W)
}

class PDAInputIO extends Bundle {
  val control = Input(new PDAControlInputBundle)
  val data = Flipped(Decoupled(UInt(1.W)))
}

class PDAControlOutputBundle extends Bundle {
  val length = UInt(8.W)
  val flag_aa = Bool()
  val flag_crc = Bool()
  val done = Bool() // Treat this as a "valid" when done is high we guarantee the other signals have their correct value
  val busy = Bool() // TODO: Set this to be state =/= s_idle
}

class PDAOutputIO extends Bundle {
  val control = Output(new PDAControlOutputBundle)
  val data = Decoupled(UInt(8.W))
}

class PacketDisassemblerIO extends Bundle {
  val in = new PDAInputIO
  val out = new PDAOutputIO
  val constants = Input(new BasebandConstants)
}

class PacketDisassembler extends Module {
  val io = IO(new PacketDisassemblerIO)

  val s_idle :: s_preamble :: s_aa :: s_pdu_header :: s_pdu_payload :: s_crc :: s_wait_dma :: Nil = Enum(7)
  val state = RegInit(s_idle)

  val aa = Reg(UInt(32.W))

  val counter = RegInit(0.U(8.W)) //counter for bytes in packet
  val counter_byte = RegInit(0.U(3.W)) //counter for bits in bytes

  //packet status
  val length = RegInit(0.U(8.W))
  val done = RegInit(false.B)
  val flag_aa = Wire(Bool())
  val flag_crc = Wire(Bool())

  //Preamble
  val preamble0 = "b10101010".U
  val preamble1 = "b01010101".U
  val preamble = Mux(aa(0), preamble1, preamble0)

  //Handshake Parameters
  val out_valid = RegInit(false.B)
  val in_ready = RegInit(false.B)

  //data registers

  val data = RegInit(VecInit(Seq.fill(8)(false.B)))

  //crc
  val crc_reset = (state === s_idle)
  val crc_data = Wire(UInt(1.W))
  val crc_valid = Wire(Bool())
  val crc_result = Wire(UInt(24.W))
  val crc_seed = io.constants.crcSeed

  //whitening
  val dewhite_reset = state === s_idle
  val dewhite_data = Wire(UInt(1.W))
  val dewhite_valid = Wire(Bool())
  val dewhite_result = Wire(UInt(1.W))
  val dewhite_seed = io.constants.whiteningSeed



  //output function
  io.out.data.bits := Mux(state === s_idle || state === s_preamble, 0.U, data.asUInt)

  done := state === s_crc && counter === 2.U && counter_byte === 7.U && io.in.data.fire()

  io.out.bits.length := length
  io.out.bits.flag_aa := flag_aa
  io.out.bits.flag_crc := flag_crc
  io.out.bits.done := done

  io.out.valid := out_valid
  io.in.ready := in_ready

  io.busy := state =/= s_idle


  when (state === s_idle) {
    when (io.in.bits.switch && io.in.valid) {//note: switch usage
      state := s_preamble
    }
  }.elsewhen (state === s_preamble) {
    when (data.asUInt === preamble) {
      state := s_aa
      counter := 0.U
      counter_byte := 0.U
    }
  }.elsewhen (state === s_aa) {
    when (counter === 3.U && io.out.fire()) {//note
      state := s_pdu_header
      counter := 0.U
      counter_byte := 0.U
    }.otherwise {
      when (io.out.fire()) {
        counter := counter + 1.U
      }
      when (io.in.fire()) {
        counter_byte := Mux(counter_byte === 7.U, 0.U, counter_byte + 1.U)
      }
    }
  }.elsewhen (state === s_pdu_header) {
    when (counter === 1.U && io.out.fire()) {//note
      state := s_pdu_payload
      counter := 0.U
      counter_byte := 0.U
    }.otherwise {
      when (io.out.fire()) {
        counter := counter + 1.U
      }
      when (io.in.fire()) {
        counter_byte := Mux(counter_byte === 7.U, 0.U, counter_byte + 1.U)
      }
    }
  }.elsewhen (state === s_pdu_payload) {
    when (counter === length - 1.U && io.out.fire()) {
      state := s_crc
      counter := 0.U
      counter_byte := 0.U
    }.otherwise {
      when (io.out.fire()) {
        counter := counter + 1.U
      }
      when (io.in.fire()) {
        counter_byte := Mux(counter_byte === 7.U, 0.U, counter_byte + 1.U)
      }
    }
  }.elsewhen (state === s_crc) {
    when (counter === 2.U && io.out.fire()) {//note
      state := s_wait_dma
      counter := 0.U
      counter_byte := 0.U
    }.otherwise {
      when (io.out.fire()) {
        counter := counter+1.U
      }
      when (io.in.fire()) {
        when (counter_byte === 7.U) {
          counter_byte := 0.U
        }.otherwise {
          counter_byte := counter_byte+1.U
        }
      }
    }
  }.elsewhen (state === s_wait_dma) {
    when (io.out.ready) {
      state := s_idle
    }
  }.otherwise {
    state := s_idle //error
  }

  //PDU_Length
  when (state === s_pdu_header && counter === 1.U && io.out.fire()) {
    length := data.asUInt
  }

  //Flag_aa
  when (state === s_aa && counter === 0.U && io.out.fire()) {//note: same as above
    when (data.asUInt =/= reg_aa(7,0)) {
      flag_aa := true.B
    }.otherwise {
      flag_aa := false.B
    }
  }.elsewhen (state === s_aa && counter === 1.U && io.out.fire()) {
    when (data.asUInt =/= reg_aa(15,8)) {
      flag_aa := true.B
    }.otherwise {
      flag_aa := false.B
    }
  }.elsewhen (state === s_aa && counter === 2.U && io.out.fire()) {
    when (data.asUInt =/= reg_aa(23,16)) {
      flag_aa := true.B
    }.otherwise {
      flag_aa := false.B
    }
  }.elsewhen (state === s_aa && counter === 3.U && io.out.fire()) {
    when (data.asUInt =/= reg_aa(31,24)) {
      flag_aa := true.B
    }.otherwise {
      flag_aa := false.B
    }
  }.otherwise {
    flag_aa := false.B
  }

  //Flag_crc
  when (state === s_crc && counter === 0.U && io.out.fire()) {//note: same as above
    when (data.asUInt =/= crc_result(7,0)) {
      flag_crc := true.B
    }.otherwise {
      flag_crc := false.B
    }
  }.elsewhen (state === s_crc && counter === 1.U && io.out.fire()) {
    when (data.asUInt =/= crc_result(15,8)) {
      flag_crc := true.B
    }.otherwise {
      flag_crc := false.B
    }
  }.elsewhen (state === s_crc && counter === 2.U && io.out.fire()) {
    when (data.asUInt =/= crc_result(23,16)) {
      flag_crc := true.B
    }.otherwise {
      flag_crc := false.B
    }
  }.otherwise {
    flag_crc := false.B
  }



  //out_valid
  when (state === s_idle || state === s_preamble) {
    out_valid := false.B
  }.elsewhen (counter_byte === 7.U && io.in.fire()) { //aa, pdu_header, pdu_payload, crc
    out_valid := true.B
  }.elsewhen (io.out.fire()) {
    out_valid := false.B
  }

  //AFIFO_Ready_w//note:check corner cases
  when (state === s_idle) {
    in_ready := false.B // TODO: this is their hack to account for the fact that "switch" in included in the decoupled
                        //  bundle, they turn ready off so they know they don't get data from a queue and then check
                        //  switch and valid
  }.elsewhen (state === s_preamble) {
    in_ready := true.B
  }.elsewhen (counter_byte === 7.U && io.in.fire()) { //aa, pdu_header, pdu_payload, crc
    in_ready := false.B
  }.elsewhen (io.out.fire()) {
    in_ready := true.B
  }

  //data
  when (state === s_pdu_header || state === s_pdu_payload || state === s_crc) {
    when (io.in.data.fire()) {
      data(counter_byte) := dewhite_result =/= 0.U
    }
  }.elsewhen (state === s_preamble) {
    when (io.in.data.fire()) {
      //data(7) := io.in.bits.data.toBools //note: subword assignment
      data(7) := io.in.data.bits.asBool()
      for(i <- 0 to 6) {//value shifting
        data(i) := data(i+1)
      }
    }
  }.elsewhen (state === s_aa) {
    when (io.in.data.fire()) {
      data(counter_byte) := io.in.data.bits.asBool()
    }
  }

  //crc
  when (state === s_pdu_header || state === s_pdu_payload) {//check corner cases
    crc_data := dewhite_result
    crc_valid := io.in.data.fire()
  }.otherwise {
    crc_data := 0.U
    crc_valid := false.B
  }


  //dewhitening
  when (state === s_pdu_header || state === s_pdu_payload || state === s_crc) {//check corner cases
    dewhite_data  := io.in.data.bits
    dewhite_valid := io.in.data.fire()
  }.otherwise {
    dewhite_data  := 0.U
    dewhite_valid := false.B
  }

  //crc instantiate
  val crc = Module(new CRC)

  crc.io.init := crc_reset
  crc.io.operand.bits := crc_data
  crc.io.operand.valid := crc_valid
  crc_result := crc.io.result
  crc.io.seed := crc_seed

  //whitening instantiate
  val white = Module(new Whitening)

  white.io.init := dewhite_reset
  white.io.operand.bits := dewhite_data
  white.io.operand.valid := dewhite_valid
  dewhite_result := white.io.result.bits
  white.io.seed := dewhite_seed

}