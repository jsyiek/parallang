package uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel

import org.scalatest.funsuite.AnyFunSuite
import uk.ac.cam.crsid.lib.Events.Listeners.TallyListener

import scala.concurrent.duration.DurationDouble

class LatencyContextTestSuite extends AnyFunSuite {
  test("Sending event ticks up penalty appropriately") {
    val latencyContext = new LatencyContext()
    latencyContext.alertAll(ALUEvent())
    assert(latencyContext.getTimestamp === ALUEvent().penalty_ps)
  }

  test("Sending MessageReadEvent appropriately raises according to the difference") {
    val latencyContext = new LatencyContext()
    val arrivalTs = 1000 /* ps */
    latencyContext.alertAll(MessageReadEvent(arrivalTs, Array(arrivalTs)))
    assert(latencyContext.getTimestamp === MessageReadEvent(arrivalTs, Array()).penalty_ps + arrivalTs)
  }

  test("LatencyContext sends updates to its listeners") {
    val latencyContext = new LatencyContext()
    val tallyListener = new TallyListener[LatencyEvent]()
    latencyContext.subscribe(tallyListener)
    latencyContext.alertAll(ALUEvent())
    assert(tallyListener.tallies(ALUEvent().getClass.getSimpleName) === 1)
  }
}
