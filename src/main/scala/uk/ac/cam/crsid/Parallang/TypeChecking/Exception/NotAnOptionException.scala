package uk.ac.cam.crsid.Parallang.TypeChecking.Exception

import uk.ac.cam.crsid.Parallang.Parser.Token
import uk.ac.cam.crsid.Parallang.TypeChecking.Type
import uk.ac.cam.crsid.lib.Exception.BlameTokenException

class NotAnOptionException(typed: Type, token: Token)
  extends BlameTokenException(
    "NotAnOptionException",
    s"Expression inside get is a $typed which is not an option.",
    token
  ) {}
