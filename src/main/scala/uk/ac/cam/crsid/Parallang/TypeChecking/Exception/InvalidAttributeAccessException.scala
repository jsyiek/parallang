package uk.ac.cam.crsid.Parallang.TypeChecking.Exception

import uk.ac.cam.crsid.Parallang.Parser.Token
import uk.ac.cam.crsid.lib.Exception.BlameTokenException

class InvalidAttributeAccessException(attr: String, structName: String, token: Token)
  extends BlameTokenException(
    "InvalidAttributeAccessException",
    s"Unknown attribute of $structName: $attr",
    token
  ){ }
