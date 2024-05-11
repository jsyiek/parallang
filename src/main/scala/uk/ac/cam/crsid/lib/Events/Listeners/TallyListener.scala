package uk.ac.cam.crsid.lib.Events.Listeners

import uk.ac.cam.crsid.lib.Events.{Event, EventListener}

import scala.collection.mutable
import scala.concurrent.duration.Duration

class TallyListener[T <: Event] extends EventListener[T] {

  val tallies: mutable.Map[String, Int] = new mutable.HashMap[String, Int]().withDefaultValue(0)
  override def alert(event: T, timestamp: Long): Unit = {
    tallies(event.getClass.getSimpleName) += 1
  }

  override def toString: String = {
    var result = "TallyListener:"
    for ((s, i) <- tallies) {
      result += s"\n\t\t\t$s: $i"
    }
    result
  }

  def apply(key: String): Int = tallies(key)
}
