package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel

import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.Value
import uk.ac.cam.crsid.lib.Collection.Context

import scala.annotation.tailrec

class OwnedMemoryContext(owner: ExecutionContext) extends Context[Value] {

  override def put(name: String, value: Value): Unit = {
    // Put writes to the top context, so we need to look to see
    // if we are unlinking something before putting it
    // That said, if we are just reassigning the same value to itself,
    // we don't want to trigger a deallocation and subsequent reallocation of the same value
    lookupTopContext(name) foreach (v => if (v != value) v.unlinkReference(this))
    super.put(name, value)
    value.linkReference(this, Some(owner))
  }

  override def update(name: String, value: Value): Unit = {
      lookupOrDie(name).makeShallowCopyOf(value)
//    super.update(name, )
  }

  def deallocateTopFrame(): Unit = context match {
    case hd :: _ => hd.values foreach {
      _.unlinkReference(this)
    }
    case _ => ()
  }

  @tailrec
  final def deallocateAllFrames(): Unit = context match {
    case _ :: _ =>
      popFrame()
      deallocateAllFrames()
    case _ => ()
  }

  override def popFrame(): Unit = {
    deallocateTopFrame()
    super.popFrame()
  }

}
