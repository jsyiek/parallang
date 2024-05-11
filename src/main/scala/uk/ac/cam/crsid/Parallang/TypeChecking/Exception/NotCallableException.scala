package uk.ac.cam.crsid.Parallang.TypeChecking.Exception

import uk.ac.cam.crsid.Parallang.Parser.Token
import uk.ac.cam.crsid.Parallang.TypeChecking.Type
import uk.ac.cam.crsid.lib.Exception.BlameTokenException

class NotCallableException(name: String, typed: Type, token: Token)
  extends BlameTokenException(
    "NotCallableException",
    s"$name is of type $typed which is not callable",
    token
  ) { }
