/* Based on (with permission) the Fall 2018 290C BLE Baseband
   https://github.com/ucberkeley-ee290c/fa18-ble/
 */

package baseband

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem

class PAInputBundle extends Bundle {
  val trigger = Bool()
  val data = UInt(8.W)
  //val crc_seed = Output(UInt(24.W))
  //val white_seed = Output(UInt(7.W))

}

class PAOutputBundle extends Bundle {
  val data = UInt(1.W)
  val done = Bool()
}


class PacketAssemblerIO extends Bundle {
  val in = Flipped(Decoupled(new PAInputBundle))
  val out = Decoupled(new PAOutputBundle)
}

class PacketAssembler extends Module {

  def stateUpdate(currentState: UInt, nextState: UInt, length: UInt, counter: UInt, counterByte: UInt, condition: Bool)= {
    val stateOut = Wire(UInt(3.W))
    val counterOut = Wire(UInt(8.W))
    val counterByteOut = Wire(UInt(3.W))
    counterOut := counter
    counterByteOut := counterByte

    when(counter === length - 1.U && counterByte === 7.U && condition) {
      stateOut := nextState
      counterOut := 0.U
      counterByteOut := 0.U
    }.otherwise {
      stateOut := currentState
      when(condition) {
        when(counterByte === 7.U) {
          counterOut := counter + 1.U
          counterByteOut := 0.U
        }.otherwise {
          counterByteOut := counterByte + 1.U
        }
      }
    }
    (stateOut, counterOut, counterByteOut)
  }

  val io = IO(new PacketAssemblerIO)

  //state parameter
  val s_idle :: s_preamble :: s_aa :: s_pdu_header :: s_pdu_payload :: s_crc :: Nil = Enum(6)
  val state = RegInit(s_idle)

  val counter = RegInit(0.U(8.W)) //counter for bytes in packet
  val counter_byte = RegInit(0.U(3.W)) //counter for bits in bytes

  val pdu_length = RegInit(0.U(8.W))

  //Preamble
  val preamble0 = "b10101010".U // flipped preamble; start with least significant bit
  val preamble1 = "b01010101".U

  //Handshake parameters
  val in_ready = RegInit(false.B) // TODO: Why is there registering for the ready and valid signals?
  val out_valid = RegInit(false.B)

  //data registers
  val data = RegInit(0.U(8.W))

  //CRC
  val crc_reset = io.in.bits.trigger && io.in.valid
  val crc_data = Wire(UInt(1.W))
  val crc_valid = Wire(Bool())
  val crc_result = Wire(UInt(24.W))
  val crc_seed = Wire(UInt(24.W))

  //whitening
  val white_reset = io.in.bits.trigger && io.in.valid
  val white_data = Wire(UInt(1.W))
  val white_valid = Wire(Bool())
  val white_result = Wire(UInt(1.W))
  val white_seed = Wire(UInt(7.W))

  //hardcode seed initiation
  crc_seed := "b010101010101010101010101".U // TODO: This should be replaced with input controlled seeding
  white_seed := "b1100101".U

  //decouple assignments
  io.in.ready := in_ready
  io.out.valid := out_valid

  //output bits
  when(state === s_idle) {
    io.out.bits.data := 0.U
  }.elsewhen(state === s_pdu_header || state === s_pdu_payload || state === s_crc) {
    io.out.bits.data := white_result
  }.otherwise {//PREAMBLE, s_aa
    io.out.bits.data := data(counter_byte)
  }

  io.out.bits.done := state === s_crc && counter === 2.U && counter_byte === 7.U && io.out.fire() // End of packet

  //State Transition with counter updates
  when(state === s_idle) {
    when(io.in.bits.trigger && io.in.valid) {
      state := s_preamble
      counter := 0.U
      counter_byte := 0.U
    }.otherwise {
      state := s_idle
    }
  }.elsewhen(state === s_preamble) {
    val (stateOut, counterOut, counterByteOut) = stateUpdate(s_preamble, s_aa, 1.U, counter, counter_byte, io.out.fire())
    state := stateOut
    counter := counterOut
    counter_byte := counterByteOut
  }.elsewhen(state === s_aa) {
    val (stateOut, counterOut, counterByteOut) = stateUpdate(s_aa, s_pdu_header, 4.U, counter, counter_byte, io.out.fire())
    state := stateOut
    counter := counterOut
    counter_byte := counterByteOut
  }.elsewhen(state === s_pdu_header) {
    val (stateOut, counterOut, counterByteOut) = stateUpdate(s_pdu_header, s_pdu_payload, 2.U, counter, counter_byte, io.out.fire())
    state := stateOut
    counter := counterOut
    counter_byte := counterByteOut
  }.elsewhen(state === s_pdu_payload) {
    val (stateOut, counterOut, counterByteOut) = stateUpdate(s_pdu_payload, s_crc, pdu_length, counter, counter_byte, io.out.fire())
    state := stateOut
    counter := counterOut
    counter_byte := counterByteOut
  }.elsewhen(state === s_crc) {
    val (stateOut, counterOut, counterByteOut) = stateUpdate(s_crc, s_idle, 3.U, counter, counter_byte, io.out.fire())
    state := stateOut
    counter := counterOut
    counter_byte := counterByteOut
  }.otherwise {
    state := s_idle //error
  }


  //PDU_Length
  when(state === s_pdu_header && counter === 1.U) {
    pdu_length := data
  }

  //in_ready //note:check corner cases
  when(state === s_aa || state === s_pdu_header || state === s_pdu_payload) {
    when(state === s_pdu_payload && counter === pdu_length-1.U && counter_byte === 7.U && io.out.fire()) {
      in_ready := false.B //special case at the end of PAYLOAD
    }.elsewhen(counter_byte === 7.U && io.out.fire()) {
      in_ready := true.B
    }.elsewhen(io.in.fire()) {
      in_ready := false.B
    }
  }.elsewhen(state === s_preamble && counter === 0.U && counter_byte === 7.U && io.out.fire()) {
    in_ready := true.B//special case at the end of PREAMBLE: s_aa starts with ready
  }.elsewhen(state === s_idle) {
    in_ready := true.B
  }.otherwise {
    in_ready := false.B
  }

  //output valid
  when(state === s_idle) {
    out_valid := false.B
  }.elsewhen(state === s_preamble) {
    when(counter === 0.U && counter_byte === 7.U && io.out.fire()) {
      out_valid := false.B//special case at the end of PREAMBLE: s_aa starts with invalid
    }.elsewhen(io.in.valid) {
      out_valid := true.B
    }
  }.elsewhen(state === s_crc) {
    when(counter === 2.U && counter_byte === 7.U && io.out.fire()) {
      out_valid := false.B//special case at the end of CRC
    }.otherwise {
      out_valid := true.B
    }
  }.otherwise {//s_aa, s_pdu_header, s_pdu_payload
    when(counter_byte === 7.U && io.out.fire()) {
      out_valid := false.B
    }.elsewhen(io.in.fire()) {
      out_valid := true.B
    }
  }

  //data
  when(state === s_aa || state === s_pdu_header || state === s_pdu_payload) {
    when(io.in.fire()) {
      data := io.in.bits.data
    }
  }.elsewhen(state === s_preamble) {
    when(io.in.valid) {
      when(io.in.bits.data(0)) {
        data := preamble1
      }.otherwise {
        data := preamble0
      }
    }
  }.elsewhen(state === s_crc) {
    when(counter === 0.U) {
      data := crc_result(7,0)
    }.elsewhen(counter === 1.U) {
      data := crc_result(15,8)
    }.elsewhen(counter === 2.U) {
      data := crc_result(23,16)
    }.otherwise {
      data := crc_result(7,0)//error
    }
  }.otherwise {//IDLE
    data := 0.U//or preserve
  }

  //Set CRC Parameters
  when(state === s_pdu_header || state === s_pdu_payload) {
    crc_data := data(counter_byte)
    crc_valid := io.out.fire()
  }.otherwise {
    crc_data := 0.U
    crc_valid := false.B
  }

  //Set Whitening Parameters
  when(state === s_pdu_header || state === s_pdu_payload) {
    white_data  := data(counter_byte)
    white_valid := io.out.fire()
  }.elsewhen(state === s_crc) {
    white_data := crc_result(counter * 8.U + counter_byte)
    white_valid := io.out.fire()
  }.otherwise {
    white_data := 0.U
    white_valid := false.B
  }


  //Instantiate CRC Module
  val crc = Module(new CRC)

  crc.io.init := crc_reset
  crc.io.operand.bits := crc_data
  crc.io.operand.valid := crc_valid
  crc_result := crc.io.result
  crc.io.seed := crc_seed

  //Instantiate Whitening Module
  val white = Module(new Whitening)

  white.io.init := white_reset
  white.io.operand.bits := white_data
  white.io.operand.valid := white_valid
  white_result := white.io.result.bits
  white.io.seed := white_seed
}