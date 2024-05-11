package uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.Listeners

import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.LatencyEvent
import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.LatencyEvent.numSamples
import uk.ac.cam.crsid.lib.Events.EventListener

import scala.collection.mutable
import scala.concurrent.duration.{Duration, DurationInt}

class TimeTallyListener extends EventListener[LatencyEvent] {

  val tallies: mutable.Map[String, Long] = new mutable.HashMap[String, Long]().withDefaultValue(0)
  val allTallies: mutable.Map[String, Array[Long]] = new mutable.HashMap[String, Array[Long]]()
  var total: Long = 0
  val allTotal: Array[Long] = Array.fill(numSamples) {0}

  override def alert(event: LatencyEvent, timestamp_ps: Long): Unit = {
    tallies(event.getClass.getSimpleName) = tallies(event.getClass.getSimpleName) + event.getPenalty(timestamp_ps)
    total = total + event.getPenalty(timestamp_ps)
    if (!allTallies.contains(event.getClass.getSimpleName)) {
      allTallies(event.getClass.getSimpleName) = Array.fill(numSamples) {0}
    }
    val samples = event.getAllPenalties(allTotal)
    for (i <- 0 until numSamples) {
      allTotal(i) += samples(i)
      allTallies(event.getClass.getSimpleName)(i) += samples(i)
    }
  }

  override def toString: String = {
    var result = s"TimeTallyListener: (total $total)"
    for ((s, i) <- tallies) {
      result += s"\n\t\t\t$s: avg: $i, ${allTallies(s).mkString("Samples(", ", ", ")")}"
    }
    result
  }

}
