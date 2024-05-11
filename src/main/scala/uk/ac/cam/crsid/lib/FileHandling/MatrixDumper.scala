package uk.ac.cam.crsid.lib.FileHandling

import java.io.{File, PrintWriter}

object MatrixDumper {

  /**
   * Dumps a predecessor matrix into a space separated file where the ith
   * row is the ith row of the predecessor matrix
   *
   * @param filepath where to dump the output
   * @param matrix the matrix to dump
   */
  def dump(filepath: String, matrix: Array[Array[Long]]): Unit = {
    val dumpFile = new File(filepath)
    val dumpWriter = new PrintWriter(dumpFile)
    try {
      dumpWriter.write((matrix map (_.mkString(" "))) mkString "\n")
    } finally {
      dumpWriter.close()
    }
  }

}
