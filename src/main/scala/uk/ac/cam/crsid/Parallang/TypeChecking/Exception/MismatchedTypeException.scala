package uk.ac.cam.crsid.Parallang.TypeChecking.Exception

import uk.ac.cam.crsid.Parallang.Parser.Token
import uk.ac.cam.crsid.Parallang.TypeChecking.Type
import uk.ac.cam.crsid.lib.Exception.BlameTokenException

class MismatchedTypeException(expectedType: Type, receivedType: Type, token: Token)
  extends BlameTokenException(
    "MismatchedTypeException",
    s"Expected $expectedType, received $receivedType",
    token
  ) { }