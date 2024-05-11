package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Listeners

import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.{AllocationMemoryEvent, DeallocationMemoryEvent, MemoryEvent}
import uk.ac.cam.crsid.lib.Events.EventListener

import scala.concurrent.duration.{Duration, DurationInt}

class PeakMemoryListener extends EventListener[MemoryEvent] {

  private var currMemory: Long = 0
  private var peakMemory: Long = 0
  private var peakedAt: Long = 0

  override def alert(event: MemoryEvent, timestamp: Long): Unit = event match {
    case AllocationMemoryEvent(b, _, _) =>
      currMemory += b
      if (currMemory > peakMemory) {
        peakMemory = currMemory
        peakedAt = timestamp
      }

    case DeallocationMemoryEvent(b, _) =>
      currMemory -= b
      if (currMemory < 0) {
        println("Something has gone wrong! Memory is less than 0!")
      }
  }

  def peak: Long = peakMemory

  override def toString: String = s"PeakMemoryListener: Peak ${peakMemory}B @ $peakedAt"
}
