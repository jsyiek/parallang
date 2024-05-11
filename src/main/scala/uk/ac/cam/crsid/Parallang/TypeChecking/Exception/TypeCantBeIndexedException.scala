package uk.ac.cam.crsid.Parallang.TypeChecking.Exception

import uk.ac.cam.crsid.Parallang.Parser.Token
import uk.ac.cam.crsid.Parallang.TypeChecking.Type
import uk.ac.cam.crsid.lib.Exception.BlameTokenException

class TypeCantBeIndexedException(observedType: Type, token: Token)
  extends BlameTokenException(
    "TypeCantBeIndexedException",
    s"${token.image} is an $observedType which can't be indexed",
    token) { }
