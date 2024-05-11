package uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.Listeners

import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.{ALUEvent, AllocationEvent, BranchEvent, CachedMemoryLookupEvent, DeallocationEvent, LatencyEvent, MessageReadEvent, MessageSendEvent}
import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.LatencyEvent.numSamples
import uk.ac.cam.crsid.lib.Events.EventListener

import scala.collection.mutable
import scala.concurrent.duration.{Duration, DurationInt}

class OptimizedTimeTallyListener extends EventListener[LatencyEvent] {

  //  val tallies: mutable.Map[String, Long] = new mutable.HashMap[String, Long]().withDefaultValue(0)
  //  val allTallies: mutable.Map[String, Array[Long]] = new mutable.HashMap[String, Array[Long]]()
  var total: Long = 0
  val allTotal: Array[Long] = Array.fill(numSamples) {0}
  val numEvents = 7
  val names = Array("ALUEvent", "MessageReadEvent", "MessageSendEvent", "AllocationEvent", "DeallocationEvent", "CachedMemoryLookupEvent", "BranchEvent")
  val new_tallies: Array[Long] = Array.fill(numEvents) {0}
  val new_allTallies: Array[Array[Long]] = Array.ofDim(numEvents, numSamples)

  override def alert(event: LatencyEvent, timestamp_ps: Long): Unit = {
    val ind = event match {
      case ALUEvent() => 0
      case MessageReadEvent(avgArrivalTimestamp_ps, allArrivalTimestamp_ps) => 1
      case MessageSendEvent(avgWirePushDuration, wirePushDuration) => 2
      case AllocationEvent() => 3
      case DeallocationEvent() => 4
      case CachedMemoryLookupEvent(latency_ps, levelOfHit) => 5
      case BranchEvent() => 6
    }
    new_tallies(ind) = new_tallies(ind) + event.getPenalty(timestamp_ps)
    total = total + event.getPenalty(timestamp_ps)
    val samples = event.getAllPenalties(allTotal)
//    if (ind == 0 || ind == 4) {
//      // there is no point duplicating ALUEvent/CacheLookup for each simulation
//      // they are the same across all
//      // therefore assume they are added after run-time.
//      return;
//    }
    for (i <- 0 until numSamples) {
      allTotal(i) += samples(i)
      new_allTallies(ind)(i) += samples(i)
    }
  }

  override def toString: String = {
    var result = s"TimeTallyListener: (total $total)"
    for ((name, i) <- (new_tallies.indices zip new_tallies)) {
      result += s"\n\t\t\t${names(name)}: avg: ${new_tallies(name)}, ${new_allTallies(name).mkString("Samples(", ", ", ")")}"
    }
    result
  }

}
