package uk.ac.cam.crsid.lib.Collection

import scala.collection.mutable

/*
 * Implements a simpler priority queue that can be efficiently used for LRU.
 * It functions like a queue, except that push an element a second time resets
 * its position in the list to the end.
 */

class PushFallPriorityQueue[T] {
  implicit class LinkedListNode(val item: T)(var prev: Option[LinkedListNode], var next: Option[LinkedListNode])

  var head: Option[LinkedListNode] = None
  var tail: Option[LinkedListNode] = None
  val locations: mutable.HashMap[T, LinkedListNode] = new mutable.HashMap()

  def push(e: T): Unit = {
    tail match {
      case None =>
        val newNode = new LinkedListNode(e)(None, None)
        head = Some(newNode)
        tail = head
        locations(e) = newNode
      case Some(t) =>
        locations.get(e) match {
          case None =>
            val newNode = new LinkedListNode(e)(Some(t), None)
            t.next = Some(newNode)
            tail = t.next
            locations(e) = newNode
          case Some(existingNode) =>
            if (head == tail && tail.contains(existingNode)) {
              return
            }

            if (head.contains(existingNode)) {
              head = existingNode.next
            }

            existingNode.prev foreach {_.next = existingNode.next}
            existingNode.next foreach {_.prev = existingNode.prev}
            tail foreach {_.next = Some(existingNode)}
            existingNode.prev = tail
            existingNode.next = None
            tail = Some(existingNode)
        }
    }
  }

  def dequeue(): Option[T] = {
    val toReturn = head.map { n =>
      locations.remove(n.item)
      n.next foreach { _.prev = None }
      n.item
    }
    if (head == tail) {
      tail = None
    }
    head = head flatMap { _.next }

    toReturn
  }

}
