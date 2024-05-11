package uk.ac.cam.crsid.Parallang.TypeChecking.Exception

import uk.ac.cam.crsid.Parallang.Parser.Token
import uk.ac.cam.crsid.lib.Exception.BlameTokenException

class RedeclarationException(variableName: String, token: Token)
  extends BlameTokenException(
    "RedeclarationException",
    s"$variableName has already been declared in this scope.",
    token
  ) { }
