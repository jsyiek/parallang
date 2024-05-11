package uk.ac.cam.crsid.Parallang.Interpreter.CommunicationModel

import breeze.stats.distributions.Gaussian
import breeze.stats.distributions.Rand.VariableSeed.randBasis
import uk.ac.cam.crsid.Parallang.Interpreter.CommunicationModel.LatencyParameterSet.minimumWirePushTime_ps
import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.LatencyEvent.numSamples
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.Value
import uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel.TimingLong._

import scala.language.postfixOps

object LatencyParameterSet {
  val minimumWirePushTime_ps: Long = 1
  // as measured with benchmark
  val overhead_ps: Long = 481749 picoseconds
  val overhead_stddev: Long = 223594 picoseconds
}

trait LatencyParameterSet {
  val hopLatency_ps: Long
  val bytesPerSecond: Long
  val hopLatency_ps_stddev: Long
  val bytesPerSecond_stddev: Long

  val bytesPerSecond_gaussian: Gaussian
  val hopLatency_gaussian: Gaussian

  def getWireLatencyTime(hopCount: Long): Long =
    hopLatency_ps * hopCount

  def getWirePushTime(v: Value): Long = {
    // Long resolution isn't great, so we convert it to picoseconds by multiplying by 1e12 before calculating
    val candidateReturn = v.totalSizeInBytes()*1000*1000*1000*1000/bytesPerSecond
    if (candidateReturn < minimumWirePushTime_ps) {
      minimumWirePushTime_ps
    } else {
      candidateReturn
    }
  }

  def getUncertainWirePushTimes(v: Value): IndexedSeq[Long] = {
    bytesPerSecond_gaussian.sample(numSamples).map(_.asInstanceOf[Long]) map {
      g =>
        val candidateReturn: Long = v.totalSizeInBytes()*1000*1000*1000*1000 / g
        if (candidateReturn < minimumWirePushTime_ps) {
          minimumWirePushTime_ps
        } else {
          candidateReturn
        }
    }
  }
}

object Datacenter extends LatencyParameterSet {
  // See paper in bibliography
  override val hopLatency_ps: Long = 6454 nanoseconds
  override val hopLatency_ps_stddev: Long = 265 nanoseconds

  // See paper in bibliography
  override val bytesPerSecond: Long = 924L * 1000L * 1000L // 924 MB
  override val bytesPerSecond_stddev: Long = 1848 * 1000

  override val bytesPerSecond_gaussian = new Gaussian(bytesPerSecond, bytesPerSecond_stddev)
  override val hopLatency_gaussian = new Gaussian(hopLatency_ps, hopLatency_ps_stddev)
}

object HighPowerInternet extends LatencyParameterSet {
  // See paper in bibliography
  override val hopLatency_ps: Long = 3365 microseconds
  override val hopLatency_ps_stddev: Long = 458 microseconds

  // See paper in bibliography
  override val bytesPerSecond: Long = 924L * 1000L * 1000L // 924 MB
  override val bytesPerSecond_stddev: Long = 1848 * 1000

  override val bytesPerSecond_gaussian = new Gaussian(bytesPerSecond, bytesPerSecond_stddev)
  override val hopLatency_gaussian = new Gaussian(hopLatency_ps, hopLatency_ps_stddev)
}

object MulticoreComputer extends LatencyParameterSet {
  // as measured by benchmark
  override val hopLatency_ps: Long = 54100 picoseconds
  override val hopLatency_ps_stddev: Long = 600 picoseconds

  // as measured by benchmark
  // 8787.504 MiB/s = 9214365794.3 bytes/s ~= 9214365794 bytes/s
  override val bytesPerSecond: Long = 9214365794L
  // 239.464 MiB/s = 251096203.26 bytes/s ~= 251096203 bytes/s
  override val bytesPerSecond_stddev: Long = 251096203L

  override val bytesPerSecond_gaussian = new Gaussian(bytesPerSecond, bytesPerSecond_stddev)
  override val hopLatency_gaussian = new Gaussian(hopLatency_ps, hopLatency_ps_stddev)
}