package uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.Listeners

import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.{CachedMemoryLookupEvent, LatencyEvent}
import uk.ac.cam.crsid.lib.Events.EventListener

import scala.collection.mutable
import scala.concurrent.duration.{Duration, DurationInt}

class CacheObservingTallyListener extends EventListener[LatencyEvent] {

  val tallies: mutable.Map[String, Long] = new mutable.HashMap[String, Long]().withDefaultValue(0)
  var total: Long = 0

  override def alert(event: LatencyEvent, timestamp_ps: Long): Unit = {
    event match {
      case event1: CachedMemoryLookupEvent =>
        tallies(s"CachedMemoryLookup(level=${event1.levelOfHit})") += 1
      case _ =>
        tallies(event.getClass.getSimpleName) += 1
    }
  }

  override def toString: String = {
    var result = s"TimeTallyListener: (total $total)"
    for ((s, i) <- tallies) {
      result += s"\n\t\t\t$s: $i"
    }
    result
  }

}
