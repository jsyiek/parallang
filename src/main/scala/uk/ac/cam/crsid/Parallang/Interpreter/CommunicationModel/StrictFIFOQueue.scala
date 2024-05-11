package uk.ac.cam.crsid.Parallang.Interpreter.CommunicationModel

import uk.ac.cam.crsid.Parallang.TypeChecking.{ArrayType, IntType, Type}

import scala.collection.mutable
import scala.concurrent.blocking

/**
 * Implements a thread-safe type-safe strict FIFO queue for incoming messages.
 * Incoming messages must be tagged with a strictly increasing sequence number
 * and are only delivered to the application when all messages with lesser
 * sequence numbers been delivered.
 */
class StrictFIFOQueue {

  private var nextRecvSeq = 0
  private val bufferedMessages = new mutable.TreeSet[Message]()(SequenceNumberMessageOrdering)
  private val queue = mutable.ArrayDeque[Message]()

  /**
   * Gives a sequenced message to the queue. The message wil be buffered
   * before delivery if it is received out of order.
   * This function does not block except for claiming the lock, but progress
   * is guaranteed if Futures invoke giveMessage and Threads invoke getFirst.
   *
   * @param m Message to deliver
   */
  def giveMessage(m: Message): Unit = this.synchronized {
    bufferedMessages.add(m)

    // bufferedMessages may become empty during repeated iteration, so we check
    // for this first using a short-circuiting conjunction.
    while (bufferedMessages.nonEmpty && bufferedMessages.head.seqNum == nextRecvSeq) {
      queue.append(bufferedMessages.head)
      bufferedMessages.remove(bufferedMessages.head)
      nextRecvSeq += 1

      // For every message we deliver, we can wake up one waiter.
      this.notify()
    }
  }

  /**
   * Gets the first successfully delivered message of a given Type.
   * This function only considers messages that have been delivered
   * and not those that have been buffered.
   *
   * Blocks if there is no message to receive.
   *
   * @param t Type of message to receive
   * @return the message
   */
  def getFirst(t: Type): Message = this.synchronized {
    var result = queue.removeFirst(m => m.t.matches(t))
    while (result.isEmpty) {
      blocking {
        this.wait()
      }
      result = queue.removeFirst(_.t.matches(t))
    }
    result.get
  }

}
