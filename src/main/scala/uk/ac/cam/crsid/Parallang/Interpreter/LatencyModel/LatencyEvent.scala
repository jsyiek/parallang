package uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel

import uk.ac.cam.crsid.lib.Events.Event
import TimingLong._
import breeze.stats.distributions.{DiscreteDistr, Gaussian, Uniform}
import breeze.stats.distributions.Rand.VariableSeed.randBasis
import uk.ac.cam.crsid.Parallang.Interpreter.CommunicationModel.LatencyParameterSet.{overhead_ps, overhead_stddev}
import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.LatencyEvent.numSamples

import language.{implicitConversions, postfixOps}
import scala.util.Random // or language._

class TimingLong(d: Long) {
  def ps: TimingLong = this
  def picoseconds: TimingLong = ps

  def ns: TimingLong = d * 1000
  def nanoseconds: TimingLong = ns

  def us: TimingLong = d * 1000000
  def microsecond: TimingLong = us
  def microseconds: TimingLong = us

  def ms: TimingLong = d * 1000000000
  def millisecond: TimingLong = ms

  def milliseconds: TimingLong = ms

  def s: TimingLong = d * 1000000000000L // 10 ** 12
  def second: TimingLong = s
  def seconds: TimingLong = s

  def get: Long = d

}

object TimingLong {

  implicit def fromTimingLong(t: TimingLong): Long = t.get

  implicit def toTimingLong(d: Long): TimingLong = new TimingLong(d)
}

/*
 * These values are based on the i7-8700k Coffee Lake
 */

object LatencyEvent {
  var numSamples: Int = 1
}

abstract class LatencyEvent(val penalty_ps: Long, val stddev: Long = 0, val calculateSamples: Boolean = true, val lowerTail: Long = 100 picoseconds) extends Event {
  val samplingDistribution = new Gaussian(penalty_ps, stddev)
  var samples: IndexedSeq[Long] =
    if (stddev == 0) (0 until numSamples) map {_ => penalty_ps}
    else if (!calculateSamples) IndexedSeq.fill(numSamples) {0}
    else samplingDistribution.sample(numSamples).map(i => if (i.asInstanceOf[Long] < lowerTail) lowerTail else i.asInstanceOf[Long])
  def getPenalty(currTs: Long): Long = penalty_ps
  def getAllPenalties(allCurrTs: Array[Long]): IndexedSeq[Long] = samples
}

// 1/2.2GHz ~ 454.54... ps ~ 455 ps
case class ALUEvent() extends LatencyEvent(455 picoseconds)
//case class MemoryLookupEvent() extends LatencyEvent(1.nanoseconds)

/*
 * levelOfHit shows the cache level whe

object TimingLong {

  implicit def fromTimingLong(t: TimingLong): Long = t.get

  implicit def toTimingLong(d: Long): TimingLong = new TimingLong(d)
}re a hit was found (1-indexed).
 * E.g., levelOfHit=2 implies it missed in the L1 but hit in the L2.
 * For an access that goes all the way to memory, the levelOfHit is -1.
 */
case class CachedMemoryLookupEvent(latency_ps: Long, levelOfHit: Int) extends LatencyEvent(latency_ps, stddev=0)

// variable timestamp based on size of message and time to push to wire
case class MessageSendEvent(avgWirePushDuration: Long, wirePushDuration: IndexedSeq[Long]) extends LatencyEvent(avgWirePushDuration+overhead_ps, overhead_stddev) {
  samples = (wirePushDuration zip samples) map {a => a._2 + (a._1 - avgWirePushDuration)}
}

// Penalty to read plus time it arrives
// LogP - take g to be 100ns
case class MessageReadEvent(avgArrivalTimestamp_ps: Long, allArrivalTimestamp_ps: Array[Long]) extends LatencyEvent(overhead_ps, overhead_stddev) {
  private def max(d1: Long, d2: Long): Long = if (d1 > d2) d1 else d2
  override def getPenalty(currTs_ps: Long): Long =
    super.getPenalty(currTs_ps) + max(0, avgArrivalTimestamp_ps - currTs_ps)

  override def getAllPenalties(allCurrTs: Array[Long]): IndexedSeq[Long] = (allCurrTs.indices) map {
    i => samples(i) + max(0, allArrivalTimestamp_ps(i) - allCurrTs(i))
  }
}
//case class EnterCallEvent() extends LatencyEvent(1.nanoseconds)
//case class LeaveCallEvent() extends LatencyEvent(1.nanoseconds)
// Assume uniform ~15-20 cycles mispredict penalty
// With a 95% accurate branch predictor, we have 0.95 * 1 + 0.05*(17.5)
// = 0.95 + 0.875 = 1.825 cycles per branch (on average)
// At 454.54... ps per cycle, we have 829.545454 ~ 830 picoseconds
case class BranchEvent() extends LatencyEvent(830 picoseconds, 0 second, false) {
  // Array(15 cycles, 16 cycles, 17 cycles, 18 cycles, 19 cycles, 20 cycles)
  private val branchMispredPenalties = Array(6818 ps, 7273 ps, 7727 ps, 8182 ps, 8636 ps, 9091 ps)
  val branchPredPenalty = 455 // 1 cycle
  samples = (0 until numSamples) map {
    _ => if (Random.nextInt(100) >= 95) {
      branchMispredPenalties(Random.nextInt(branchMispredPenalties.length))
    } else {
      branchPredPenalty
    }
  }
}


case class AllocationEvent() extends LatencyEvent(0 picoseconds, 146243 picoseconds, lowerTail = 455 ps) {
//  samples = samples map { i =>
//    var new_i = i
//    while (new_i < 0) {
//      new_i = samplingDistribution.sample(1).map(i => i.asInstanceOf[Long])(0)
//    }
//    new_i
//  }
}

case class DeallocationEvent() extends LatencyEvent(0 picoseconds, 271864 picoseconds, lowerTail = 455 ps) { }
