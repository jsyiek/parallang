package uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.Listeners

import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.{LatencyEvent, MessageReadEvent, MessageSendEvent}
import uk.ac.cam.crsid.lib.Events.EventListener

import scala.collection.mutable
import scala.concurrent.duration.{Duration, DurationInt}

class ReceiveTriggeredTallyListener extends EventListener[LatencyEvent] {

  private val tallies: mutable.Map[String, Int] = new mutable.HashMap[String, Int]().withDefaultValue(0)
  private var triggered: Boolean = false
  private var firstTimestamp: Long = 0
  private var lastTimestamp: Long = 0

  override def alert(event: LatencyEvent, timestamp: Long): Unit = {
    val priorTriggered = triggered
    triggered = triggered || event.isInstanceOf[MessageReadEvent] && event.asInstanceOf[MessageReadEvent].avgArrivalTimestamp_ps > 0 /* ps */
    if (triggered & !priorTriggered) firstTimestamp = timestamp
    lastTimestamp = timestamp
    tallies(event.getClass.getName) += 1 * (if (triggered) 1 else 0)
  }

  override def toString: String = {
    var result = s"MessageTriggeredTallyListener: (total ${lastTimestamp - firstTimestamp})"
    for ((s, i) <- tallies) {
      result += s"\n\t\t\t$s: $i"
    }
    result
  }
}
