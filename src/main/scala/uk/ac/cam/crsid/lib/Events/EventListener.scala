package uk.ac.cam.crsid.lib.Events

import scala.concurrent.duration.Duration

trait EventListener[T <: Event] {
  def alert(event: T, timestamp_ps: Long): Unit
}
