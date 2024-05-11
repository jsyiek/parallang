package uk.ac.cam.crsid.Parallang.TypeChecking.Exception

import uk.ac.cam.crsid.Parallang.Parser.Token
import uk.ac.cam.crsid.Parallang.TypeChecking.Type
import uk.ac.cam.crsid.lib.Exception.BlameTokenException

class NotAStructException(typed: Type, token: Token)
  extends BlameTokenException(
    "NotAStructException",
    s"$typed is not a struct. You cannot create `new` structs or access attributes with `.`",
    token
  ) { }
