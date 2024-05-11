package uk.ac.cam.crsid.Parallang.TypeChecking.Exception

import uk.ac.cam.crsid.Parallang.Parser.Token
import uk.ac.cam.crsid.lib.Exception.BlameTokenException

class UndefinedStructException(name: String, token: Token)
  extends BlameTokenException("UndefinedStructException", s"$name has not been declared.", token) {

}
