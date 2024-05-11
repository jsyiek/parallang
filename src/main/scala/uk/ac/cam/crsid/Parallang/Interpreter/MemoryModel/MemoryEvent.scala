package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel

import uk.ac.cam.crsid.lib.Events.Event

abstract class MemoryEvent extends Event

case class AllocationMemoryEvent(bytes: Long, parentReported: Boolean=false, forceReport: Boolean=false) extends MemoryEvent

// integers, empty arrays, and empty structs do not have heap dependents.
// deallocations without heap dependents are not reported.
// the structure containing a heap dependent reports the deallocation.
case class DeallocationMemoryEvent(bytes: Long, hasNoHeapDependents: Boolean) extends MemoryEvent
