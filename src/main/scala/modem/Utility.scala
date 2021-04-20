package modem

import chisel3.experimental.FixedPoint
import chisel3._

object Utility {
  def detectEdge(x: Bool) = x =/= RegNext(x)
  def risingedge(x: Bool) = x && !RegNext(x)
  def fallingedge(x: Bool) = !x && RegNext(x)
  def roundTowardsZero(x: FixedPoint) = (x.abs().asSInt() >> x.binaryPoint.get)(x.getWidth - x.binaryPoint.get - 1, 0).asSInt() * Mux(x(x.getWidth - 1).asBool(), (-1).S, 1.S)

}
