package uk.ac.cam.crsid.lib.FileHandling

import uk.ac.cam.crsid.lib.Exception.InvalidFileException

object EdgesReader {

  /**
   * Reads a file of edges and outputs an array of edges
   *
   * @param filepath path of file to read
   * @return number of unique nodes and an array of 3-length arrays representing edges (node 1, node 2, length in mm).
   *         suitable to be sent to the parallel machine for expansion.
   */
  def readEdges(filepath: String): (Long, Array[Array[Long]]) = {
    val file = io.Source.fromFile(filepath)
    val lines = file.getLines
    val longsList = lines map (s => ("""\d+""".r findAllIn s).toList) filter (_.nonEmpty) map { _ map { _.toLong }}
    val arr = (longsList map {
      // u, v are nodeIDs, and l is length in millimeters
        case id :: u :: v :: l :: List() => Array(u, v, l)
        case l => throw new InvalidFileException(s"Expected three items on this row, got ${l.length}")
    }).toArray

    val maxId = (arr foldLeft 0L) ((m, a) => {
      a match {
        case Array(u, v, _) => if (u > v && u > m) u else if (v > u && v > m) v else m
      }
    })

    (maxId+1, arr)
  }

}
