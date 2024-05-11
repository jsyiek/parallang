package uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.Exception

import uk.ac.cam.crsid.Parallang.Parser.Token
import uk.ac.cam.crsid.lib.Exception.BlameTokenException

class OutOfBoundsArrayAccessException(index: Int, length: Int, blameToken: Token) extends BlameTokenException(
  "OutOfBoundsArray",
  s"PE tried to access an array at index $index but its maximum length is $length.",
  blameToken
) {}
