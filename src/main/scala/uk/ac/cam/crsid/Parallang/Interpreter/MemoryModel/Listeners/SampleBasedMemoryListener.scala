package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Listeners

import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.{AllocationMemoryEvent, DeallocationMemoryEvent, MemoryEvent}
import uk.ac.cam.crsid.lib.Events.EventListener

class SampleBasedMemoryListener(frequency_ps: Long) extends EventListener[MemoryEvent] {

  private var currMemory: Long = 0
  private var samples: List[(Long, Long)] = List.empty
  private var last_sample = -1 * frequency_ps

  override def alert(event: MemoryEvent, timestamp: Long): Unit = {
    event match {
      case AllocationMemoryEvent(b, _, _) =>
        currMemory += b

      case DeallocationMemoryEvent(b, _) =>
        currMemory -= b
        if (currMemory < 0) {
          println("Something has gone wrong! Memory is less than 0!")
        }
    }

    if (timestamp - last_sample > frequency_ps) {
      samples = (timestamp, currMemory) :: samples
      last_sample = timestamp
    }
  }

  override def toString: String = s"SampleBasedMemoryListener: Samples (ts, bytes) $samples"
}
