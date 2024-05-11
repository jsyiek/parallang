package uk.ac.cam.crsid.lib.Exception

import uk.ac.cam.crsid.Parallang.Parser.Token

abstract case class BlameTokenException(name: String, message: String, token: Token) extends RuntimeException {
  override def toString: String = s"($name) $message: At line ${token.beginLine} column ${token.beginColumn}"
}
