package uk.ac.cam.crsid.Parallang.TypeChecking.Exception

import uk.ac.cam.crsid.Parallang.Parser.Token
import uk.ac.cam.crsid.lib.Exception.BlameTokenException

class IncorrectArgumentNumberException(functionName: String,
                                       correctNumber: Int,
                                       incorrectNumber: Int,
                                       token: Token)
  extends BlameTokenException(
    "IncorrectArgumentNumberException",
    s"$functionName expects $correctNumber arguments. However, $incorrectNumber was passed.",
    token
  ) {

}
