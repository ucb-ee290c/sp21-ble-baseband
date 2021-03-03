package baseband

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{VerilatorBackendAnnotation, TreadleBackendAnnotation, WriteVcdAnnotation}
import org.scalatest.flatspec.AnyFlatSpec

class PacketAssemblerTest extends AnyFlatSpec with ChiselScalatestTester {
  it should "Elaborate packet assembler" in {
    test(new PacketAssembler).withAnnotations(Seq(TreadleBackendAnnotation, WriteVcdAnnotation)) { c =>
      c.clock.step(10)
    }
  }
}