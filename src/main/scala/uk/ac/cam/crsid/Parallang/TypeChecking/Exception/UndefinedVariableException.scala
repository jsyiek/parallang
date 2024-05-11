package uk.ac.cam.crsid.Parallang.TypeChecking.Exception

import uk.ac.cam.crsid.Parallang.Parser.Token
import uk.ac.cam.crsid.lib.Exception.BlameTokenException

class UndefinedVariableException(name: String, token: Token)
  extends BlameTokenException("UndefinedVariableException", s"Variable ${name} has not been defined", token) { }
