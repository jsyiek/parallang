package uk.ac.cam.crsid.Parallang.Interpreter.LatencyModel

import org.scalatest.funsuite.AnyFunSuite
import TimingLong._
import language.{implicitConversions, postfixOps}

class TimingLongTestSuite extends AnyFunSuite {

  test("Timing long conversions work") {
    assert(1.ps.toLong === 1)
    assert(1.picoseconds.toLong === 1)
    assert(1.ns.toLong === 1000)
    assert(1.nanoseconds.toLong === 1000)
    assert(1.us.toLong === 1000000)
    assert(1.microseconds.toLong === 1000000)
    assert(1.microsecond.toLong === 1000000)
    assert(1.ms.toLong === 1000000000)
    assert(1.millisecond.toLong === 1000000000)
    assert(1.milliseconds.toLong === 1000000000)
    assert(1.s.toLong === 1000000000000L)
    assert(1.second.toLong === 1000000000000L)
    assert(1.seconds.toLong === 1000000000000L)
  }

}
