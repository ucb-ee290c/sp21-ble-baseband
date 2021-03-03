package baseband

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config.Config
import freechips.rocketchip.diplomacy.{InModuleBody, LazyModule}
import freechips.rocketchip.subsystem.{BaseSubsystem, SystemBusKey}

import chipyard._
import chipyard.iobinders._
import chipyard.harness._

trait CanHavePeripheryBLEBasebandModem { this: BaseSubsystem =>
  val baseband = p(BLEBasebandModemKey).map { params =>
    val baseband = LazyModule(new BLEBasebandModem(params, fbus.beatBytes))
    pbus.toVariableWidthSlave(Some("baseband")) { baseband.mmio }
    fbus.fromPort(Some("baseband"))() := baseband.mem

    baseband.module.io
  }
}

class WithBLEBasebandModem extends Config((site, here, up) => {
  case BLEBasebandModemKey => Some(BLEBasebandModemParams())
})

class WithBLEBasebandModemPunchthrough extends OverrideIOBinder({
  (system: CanHavePeripheryBLEBasebandModem) => {
    val ports: Seq[BLEBasebandModemAnalogIO] = system.baseband.map({ a =>
      val analog = IO(new BLEBasebandModemAnalogIO).suggestName("baseband")
      analog <> a
      analog
    }).toSeq
    (ports, Nil)
  }
})

class WithBLEBasebandModemTiedOff extends OverrideHarnessBinder({
  (system: CanHavePeripheryBLEBasebandModem, th: HasHarnessSignalReferences, ports: Seq[BLEBasebandModemAnalogIO]) => {
    ports.map { p => {
      p.modemClock := th.harnessClock
      p.data.rx.i := 0.U
      p.data.rx.i := 0.U
      p.data.tx.pllReady := true.B
    }}
  }
})
