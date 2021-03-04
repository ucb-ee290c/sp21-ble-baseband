/* Based on (with permission) the Fall 2018 290C BLE Baseband
   https://github.com/ucberkeley-ee290c/fa18-ble/
 */

package baseband

import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.subsystem.BaseSubsystem

// We trigger the PA by sending it the Access Address we wish to write a packet for and the length of the PDU body,
// which is known from the RoCC command we are sent (R[rs2] - 2)
class PAControlInputBundle extends Bundle {
  val aa = UInt(32.W)
  val pduLength = UInt(8.W)
}

class PAInputIO extends Bundle {
  val control = Flipped(Decoupled(new PAControlInputBundle))
  val data = Flipped(Decoupled(UInt(8.W)))
}

class PAControlOutputBundle extends Bundle {
  val busy = Bool()
  val done = Bool()
}

class PAOutputIO extends Bundle {
  val control = Output(new PAControlOutputBundle)
  val data = Decoupled(UInt(1.W))
}


class PacketAssemblerIO extends Bundle {
  val in = new PAInputIO
  val out = new PAOutputIO
  val constants = Input(new BasebandConstants)
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

  // State
  val s_idle :: s_preamble :: s_aa :: s_pdu_header :: s_pdu_payload :: s_crc :: Nil = Enum(6)
  val state = RegInit(s_idle)

  // Internal Counters
  val counter = RegInit(0.U(8.W)) // Counts bytes in a given message component
  val counter_byte = RegInit(0.U(3.W)) // Counts bit place within a given byte
  // TODO: Add another counter to interface with some arbitrary width interface in beatBytes bytes

  // Data registers
  val data = RegInit(0.U(8.W))
  val aa = RegInit(0.U(32.W))
  val pdu_length = RegInit(0.U(8.W))

  // Preambles: Reversed since we read LSB to MSB
  val preamble0 = "b10101010".U
  val preamble1 = "b01010101".U
  val preamble = Mux(aa(0), preamble1, preamble0)

  // CRC
  val crc_reset =  io.in.control.fire()
  val crc_data = Wire(UInt(1.W))
  val crc_valid = Wire(Bool())
  val crc_result = Wire(UInt(24.W))
  val crc_seed = io.constants.crcSeed

  // Whitening
  val white_reset = io.in.control.fire()
  val white_data = Wire(UInt(1.W))
  val white_valid = Wire(Bool())
  val white_result = Wire(UInt(1.W))
  val white_seed = io.constants.whiteningSeed

  // Handshake parameters
  val data_in_ready = Reg(Bool())
  val data_out_valid = Reg(Bool())
  val data_out_fire = io.out.data.fire()
  val data_in_fire = io.in.data.fire()

  io.in.data.ready := data_in_ready
  io.out.data.valid := data_out_valid

  // Input data bits
  //io.in.data.ready :=

  // Input control bits
  io.in.control.ready := state === s_idle

  // Output data
  val done = Reg(Bool())
  io.out.control.done := done
  io.out.data.bits := Mux(state === s_pdu_header || state === s_pdu_payload || state === s_crc, white_result, data(counter_byte))

  // Output control
  io.out.control.busy := state =/= s_idle

  // State Transition with counter updates
  when(state === s_idle) {
    data_in_ready := false.B
    data_out_valid := false.B
    done := false.B

    when(io.in.control.fire) {
      state := s_preamble
      counter := 0.U
      counter_byte := 0.U
      aa := io.in.control.bits.aa
      pdu_length := io.in.control.bits.pduLength
    }
  }.elsewhen(state === s_preamble) {
    val (stateOut, counterOut, counterByteOut) = stateUpdate(s_preamble, s_aa, 1.U, counter, counter_byte, data_out_fire)
    state := stateOut
    counter := counterOut
    counter_byte := counterByteOut
    data_out_valid := true.B
    when (stateOut === s_aa) {
      data := aa(7,0)
    }.otherwise {
      data := preamble
    }
  }.elsewhen(state === s_aa) {
    val (stateOut, counterOut, counterByteOut) = stateUpdate(s_aa, s_pdu_header, 4.U, counter, counter_byte, data_out_fire)
    state := stateOut
    counter := counterOut
    counter_byte := counterByteOut

    when(counterOut === 1.U) {
      data := aa(15,8)
    }.elsewhen(counterOut === 2.U) {
      data := aa(23,16)
    }.elsewhen(counterOut === 3.U) {
      data := aa(31,24)
    }
    when (stateOut === s_pdu_header) {
      data_in_ready := true.B // Start PDU header with an in ready (first time we need input data)
      data_out_valid := false.B // Start PDU header with a out invalid (need to get input data first)
    }
  }.elsewhen(state === s_pdu_header) {
    val (stateOut, counterOut, counterByteOut) = stateUpdate(s_pdu_header, s_pdu_payload, 2.U, counter, counter_byte, data_out_fire)
    state := stateOut
    counter := counterOut
    counter_byte := counterByteOut

    when(counter_byte === 7.U && data_out_fire) { // We have cleared the last output bit from this byte
      data_in_ready := true.B
      data_out_valid := false.B
    }.elsewhen(data_in_fire) {
      data_in_ready := false.B // We have received a new input byte
      data_out_valid := true.B
    }
  }.elsewhen(state === s_pdu_payload) {
    val (stateOut, counterOut, counterByteOut) = stateUpdate(s_pdu_payload, s_crc, pdu_length, counter, counter_byte, data_out_fire)
    state := stateOut
    counter := counterOut
    counter_byte := counterByteOut

    when (stateOut === s_crc) {
      data_in_ready := false.B // Our next state is CRC, we do not need more data
      data_out_valid := true.B // The output is fully present in CRC
    }.elsewhen (counter_byte === 7.U && data_out_fire) { // We have cleared the last output bit from this byte
      data_in_ready := true.B
      data_out_valid := false.B
    }.elsewhen (data_in_fire) { // We have received a new input byte
      data_in_ready := false.B
      data_out_valid := true.B
    }
  }.elsewhen(state === s_crc) {
    val (stateOut, counterOut, counterByteOut) = stateUpdate(s_crc, s_idle, 3.U, counter, counter_byte, data_out_fire)
    state := stateOut
    counter := counterOut
    counter_byte := counterByteOut

    when (stateOut === s_idle) {
      done := true.B // We are done on our transition to idle
      data_out_valid := false.B
    }
  }.otherwise {
    state := s_idle
  }

  //data
  when(state === s_pdu_header || state === s_pdu_payload) {
    when(data_in_fire) {
      data := io.in.data.bits
    }
  }.elsewhen(state === s_crc) {
    when(counter === 0.U) {
      data := crc_result(7,0)
    }.elsewhen(counter === 1.U) {
      data := crc_result(15,8)
    }.elsewhen(counter === 2.U) {
      data := crc_result(23,16)
    }
 }.elsewhen(state === s_idle) { // Idle
   data := 0.U // TODO: Possibly can be eliminated
 }

  //Set CRC Parameters
  when(state === s_pdu_header || state === s_pdu_payload) {
    crc_data := data(counter_byte)
    crc_valid := data_out_fire
  }.otherwise {
    crc_data := 0.U
    crc_valid := false.B
  }

  //Set Whitening Parameters
  when(state === s_pdu_header || state === s_pdu_payload) {
    white_data  := data(counter_byte)
    white_valid := data_out_fire
  }.elsewhen(state === s_crc) {
    white_data := crc_result((counter << 3).asUInt() + counter_byte)
    white_valid := data_out_fire
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