package uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel

import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.LatencyEvent.numSamples
import uk.ac.cam.crsid.lib.Events.EventGenerator

import scala.concurrent.duration.{Duration, DurationDouble, DurationInt}

class LatencyContext extends EventGenerator[LatencyEvent] {

  private var avg_timestamp_ps: Long = 0
  private val all_timestamps_ps: Array[Long] = Array.fill(LatencyEvent.numSamples){0}

  override def toString: String = s"Latency: Avg: $avg_timestamp_ps, ${all_timestamps_ps.mkString("Samples(", ", ", ")")}" + (if (listeners.nonEmpty) "\n\t\t" else "") + listeners.mkString("\n\t\t")

  override def currTimestamp(): Long = getTimestamp

  def getTimestamp: Long = avg_timestamp_ps
  def getAllTimestamps: Array[Long] = all_timestamps_ps.clone()

  override def alertAll(event: LatencyEvent): Unit = {
    super.alertAll(event)
    avg_timestamp_ps += event.getPenalty(avg_timestamp_ps)
    val penalties = event.getAllPenalties(all_timestamps_ps)
    for {
      i <- 0 until numSamples
    } {
      all_timestamps_ps(i) += penalties(i)
    }
  }

}
