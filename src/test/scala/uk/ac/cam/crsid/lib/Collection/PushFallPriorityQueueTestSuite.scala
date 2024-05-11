package uk.ac.cam.crsid.lib.Collection

import org.scalatest.funsuite.AnyFunSuite

class PushFallPriorityQueueTestSuite extends AnyFunSuite {

  test("Elements pushed in are popped out in the same order") {
    val p = new PushFallPriorityQueue[Long]()
    p.push(1)
    p.push(2)
    assert(p.dequeue().contains(1))
    assert(p.dequeue().contains(2))
    assert(p.dequeue().isEmpty)
  }

  test("Elements pushed in multiple times are moved to the back of the queue") {
    val p = new PushFallPriorityQueue[Long]()
    p.push(1)
    p.push(2)
    p.push(3)
    p.push(1)
    p.push(4)
    assert(p.dequeue().contains(2))
    assert(p.dequeue().contains(3))
    assert(p.dequeue().contains(1))
    assert(p.dequeue().contains(4))
    assert(p.dequeue().isEmpty)
  }

  test("Complex interleaving of elements test") {
    val p = new PushFallPriorityQueue[Long]()
    p.push(1) // [1]
    p.push(2) // [1, 2]
    p.push(1) // [2, 1]
    p.push(3) // [2, 1, 3]
    p.push(2) // [1, 3, 2]
    p.push(4) // [1, 3, 2, 4]
    p.push(5) // [1, 3, 2, 4, 5]
    p.push(1) // [3, 2, 4, 5, 1]
    p.push(4) // [3, 2, 5, 1, 4]
    p.push(6) // [3, 2, 5, 1, 4, 6]
    p.push(4) // [3, 2, 5, 1, 6, 4]
    assert(p.dequeue().contains(3))
    assert(p.dequeue().contains(2))
    assert(p.dequeue().contains(5))
    assert(p.dequeue().contains(1))
    assert(p.dequeue().contains(6))
    assert(p.dequeue().contains(4))
    assert(p.dequeue().isEmpty)
  }

}
