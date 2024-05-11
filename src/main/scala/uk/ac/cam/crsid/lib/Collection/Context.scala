package uk.ac.cam.crsid.lib.Collection

import uk.ac.cam.crsid.lib.Exception._

import scala.collection.mutable

// A general class used in type checking and in execution to maintain
// a well-scoped context of terms.
class Context[T] {

  protected var context: List[mutable.Map[String, T]] = List()

  def lookup(name: String): Option[T] = context.find(_.contains(name)).flatMap(m => m.get(name))

  def lookupOrDie(name: String): T = lookup(name) match {
    case None => throw new NameDoesNotExistException(s"Name not in context: $name. $this")
    case Some(t) => t
  }

//  def getContextContaining(name: String): mutable.Map[String, T] = context.find(_.contains(name)).get

  def lookupTopContext(name: String): Option[T] = context.headOption.flatMap(m => m.get(name))

  // There should always be a map in the context.
  // We intentionally leaves this inexhaustive as no other case should be reached.
  def put(name: String, value: T): Unit = context match {
    case hd :: _ => hd(name) = value
    case _ => throw new EmptyContextException("Cannot put onto an empty context.")
  }

  /*
   * Check if any frame contains a string
   * @param name: the name to check
   * @returns boolean, if the string is in any frame
   */
  def contains(name: String): Boolean = context.exists(_.contains(name))

  /*
   * Update an existing value in the map.
   *
   * @param name: the name to update
   * @throws NameDoesNotExistException if the name is in no context
   */
  def update(name: String, value: T): Unit =
    context.find(_.contains(name)) match {
      case None => throw new NameDoesNotExistException(s"Name not in context: $name. $this")
      case Some(m) => m.put(name, value)
    }

  /*
   * Puts a set of mappings onto top frame of the context.
   */
  def putMultiple(mappings: mutable.Map[String, T]): Unit = context match {
    case hd :: tl => context = (hd ++ mappings) :: tl
    case _ => throw new EmptyContextException("Cannot put onto an empty context.")
  }

  /*
   * Pushes a new frame onto the context.
   */
  def pushFrame(): Unit = context = mutable.Map[String, T]() :: context

  /*
   * Pops the top frame of the context.
   */
  def popFrame(): Unit = context match {
    case _ :: tls => context = tls
    case _ => throw new EmptyContextException("Cannot pop from an empty context.")
  }

  def withinFrame[A](body: => A): A = {
    pushFrame()
    try {
      body
    } finally {
      popFrame()
    }
  }

  override def toString: String = "Context:\n"+context.mkString("\n")
}


