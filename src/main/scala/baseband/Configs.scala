package baseband

import chisel3._
import chisel3.util._
import chisel3.experimental._
import freechips.rocketchip.config.Config
import freechips.rocketchip.diplomacy.{InModuleBody, LazyModule}
import freechips.rocketchip.subsystem.BaseSubsystem

trait CanHavePeripheryBLEBasebandModem { this: BaseSubsystem =>
  val baseband = p(BLEBasebandModemKey).map { params =>
    val baseband = LazyModule(new BLEBasebandModem(params, fbus.beatBytes))

    pbus.toVariableWidthSlave(Some("baseband")) { baseband.mmio }
    fbus.fromPort(Some("baseband"))() := baseband.mem
    ibus.fromSync := baseband.intnode

    val io = InModuleBody {
      val io = IO(new BLEBasebandModemAnalogIO(params)).suggestName("baseband")
      io <> baseband.module.io
      io
    }
    io
  }
}

class WithBLEBasebandModem extends Config((site, here, up) => {
  case BLEBasebandModemKey => Some(BLEBasebandModemParams())
})

/* Note: The following are commented out as they rely on importing chipyard, which no
         generator can do without having a circular import. They should  be added to
         files in: <chipyard root>/generators/chipyard/src/main/scala/<file>

         To use, you should then add the following to your config:
           new baseband.WithBLEBasebandModem ++
           new chipyard.iobinders.WithBLEBasebandModemPunchthrough ++
           new chipyard.harness.WithBLEBasebandModemTiedOff ++

         Finally add the following to DigitalTop.scala:
           with baseband.CanHavePeripheryBLEBasebandModem
*/

/* Place this in IOBinders.scala for use
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
*/

/* Note: Place this in HarnessBinders.scala for use
class WithBLEBasebandModemTiedOff extends OverrideHarnessBinder({
  (system: CanHavePeripheryBLEBasebandModem, th: HasHarnessSignalReferences, ports: Seq[BLEBasebandModemAnalogIO]) => {
    ports.map { p => {
      p.modemClock := th.harnessClock
      p.data.rx.i := 0.U
      p.data.rx.q := 0.U
      p.data.tx.pllReady := true.B
    }}
  }
})
 */
