package uk.ac.cam.crsid.lib.Events

import scala.collection.mutable
import scala.concurrent.duration.Duration

trait EventGenerator[T <: Event] {

  protected val listeners: mutable.HashSet[EventListener[T]] = new mutable.HashSet[EventListener[T]]()

  def subscribe(listener: EventListener[T]): Unit = listeners.add(listener)

  def alertAll(event: T): Unit = {
    for (l <- listeners) {
      l.alert(event, currTimestamp())
    }
  }

  protected def currTimestamp(): Long
}
