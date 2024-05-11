package uk.ac.cam.crsid.Parallang.Interpreter.CommunicationModel

import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.Value
import uk.ac.cam.crsid.Parallang.TypeChecking.Type

import scala.concurrent.duration.Duration

case class Message(v: Value, t: Type, avgSendTs_ps: Long, allArrivalTs_ps: Array[Long], seqNum: Int) {
//  def withDelay(delay: Long): Message = Message(v, t, arrivalTs_ps + delay, seqNum)
}

object SequenceNumberMessageOrdering extends Ordering[Message] {
  def compare(a: Message, b: Message): Int = a.seqNum compare b.seqNum
}