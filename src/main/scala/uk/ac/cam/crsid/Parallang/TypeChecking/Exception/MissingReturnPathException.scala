package uk.ac.cam.crsid.Parallang.TypeChecking.Exception

import uk.ac.cam.crsid.Parallang.Parser.Token
import uk.ac.cam.crsid.lib.Exception.BlameTokenException

class MissingReturnPathException(functionName: String, token: Token)
  extends BlameTokenException(
    "MissingReturnPathException",
    s"Function $functionName does not return a value down all control paths",
    token
  ) {

}
