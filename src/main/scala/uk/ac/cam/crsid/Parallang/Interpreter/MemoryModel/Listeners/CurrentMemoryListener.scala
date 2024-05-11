package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Listeners

import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.{AllocationMemoryEvent, DeallocationMemoryEvent, MemoryEvent}
import uk.ac.cam.crsid.lib.Events.EventListener

class CurrentMemoryListener extends EventListener[MemoryEvent] {

  private var currMemory: Long = 0

  override def alert(event: MemoryEvent, timestamp: Long): Unit = event match {
    case AllocationMemoryEvent(b, _, _) =>
      currMemory += b

    case DeallocationMemoryEvent(b, _) =>
      currMemory -= b
      if (currMemory < 0) {
        println("Something has gone wrong! Memory is less than 0!")
      }
  }

  def currentMemory: Long = currMemory

  override def toString: String = s"CurrentMemoryListener: ${currMemory}B"
}
