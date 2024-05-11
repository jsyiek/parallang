package uk.ac.cam.crsid.test_utils

import java.io.ByteArrayInputStream
import java.nio.charset.Charset

object TestUtils {
  def toStream(program: String): java.io.InputStream =
    new ByteArrayInputStream(
      program.getBytes(
        Charset.forName("UTF-8")
      )
    )

  def toExportableStream(expressionToExport: String): java.io.InputStream =
    toStream(s"send ($expressionToExport) -> external;")

  def toExportableStream(preamble: String, expressionToExport: String): java.io.InputStream =
    toStream(s"$preamble; send ($expressionToExport) -> external;")
}
