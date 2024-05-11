package apsp

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks
import uk.ac.cam.crsid.Parallang.Interpreter.CommunicationModel.InterconnectionNetwork
import uk.ac.cam.crsid.Parallang.Interpreter.MemoryModel.Values.ArrayValue
import uk.ac.cam.crsid.Parallang.TypeChecking.{ArrayType, IntType}

class AlgorithmCorrectnessTestSuite extends AnyFunSuite with TableDrivenPropertyChecks {

  def numNodesFrom(edgeList: Array[Array[Long]]): Int = {
    val max_edge = edgeList maxBy (v => if (v(0) > v(1)) v(0) else v(1))
    val max_node_id: Int = (if (max_edge(0) > max_edge(1)) max_edge(0) else max_edge(1)).asInstanceOf[Int]
    max_node_id + 1
  }

  def distanceMatrixFrom(edgeList: Array[Array[Long]]): Array[Array[Long]] = {
    val max_node_id = numNodesFrom(edgeList)
    val distanceMatrix: Array[Array[Long]] = Array.fill[Long](max_node_id, max_node_id) { Long.MaxValue}
    for (u <- edgeList) {
      distanceMatrix(u(0).asInstanceOf[Int])(u(1).asInstanceOf[Int]) = u(2)
    }
    for (u <- distanceMatrix.indices) {
      distanceMatrix(u)(u) = 0
    }
    distanceMatrix
  }

  def predecessorFrom(distanceMatrix: Array[Array[Long]]): Array[Array[Long]] =
    Array.tabulate[Long](distanceMatrix.length, distanceMatrix.length) { (u, v) => if (distanceMatrix(u)(v) == Long.MaxValue) v else u }

  def successorFrom(distanceMatrix: Array[Array[Long]]): Array[Array[Long]] =
    Array.tabulate[Long](distanceMatrix.length, distanceMatrix.length) { (u, v) => if (distanceMatrix(u)(v) == Long.MaxValue) u else v }

  // solves APSP and populates a distance, predecessor, and successor matrix from an edge list
  // returns (distanceMatrix, predecessor, successor)
  def floydWarshall(edgeList: Array[Array[Long]]): (Array[Array[Long]], Array[Array[Long]], Array[Array[Long]]) = {
    val distance = distanceMatrixFrom(edgeList)
    val predecessor = predecessorFrom(distance)
    val successor = successorFrom(distance)

    val v = distance.length
    for (k <- 0 until v)
      for (i <- 0 until v)
        for (j <- 0 until v) {
          if (distance(i)(k) != Long.MaxValue && distance(k)(j) != Long.MaxValue && distance(i)(j) > distance(i)(k) + distance(k)(j)) {
            distance(i)(j) = distance(i)(k) + distance(k)(j)
            predecessor(i)(j) = predecessor(k)(j)
            successor(i)(j) = successor(i)(k)
          }
        }
    (distance, predecessor, successor)
  }

  // Exports the Distance and Predecessor/Successor
  def runParallangProgram(script: String, edgeList: Array[Array[Long]], q: Int, numNodes: Int): (Array[Array[Long]], Array[Array[Long]]) = {

    InterconnectionNetwork.resetLengthWidth(q, q, doPrint=true)
    InterconnectionNetwork.torus()

    InterconnectionNetwork.broadcastIntAsExternal(q)

    InterconnectionNetwork.sendArrayAsExternal((0, 0), edgeList.asInstanceOf[Array[Any]])
    val numNodes = numNodesFrom(edgeList)
    InterconnectionNetwork.sendIntAsExternal((0, 0), numNodes)

    InterconnectionNetwork.launchWith(script)

    val C = InterconnectionNetwork.recvAsExternal((0, 0), ArrayType(ArrayType(IntType())))
    val P = InterconnectionNetwork.recvAsExternal((0, 0), ArrayType(ArrayType(IntType())))

    val Cmatr = ArrayValue.matrixToScalaArray(C.v.asInstanceOf[ArrayValue])
    val Pmatr = ArrayValue.matrixToScalaArray(P.v.asInstanceOf[ArrayValue])
    (Cmatr dropRight (Cmatr.length - numNodes) map (a => a.dropRight(a.length - numNodes)) ,
      Pmatr dropRight (Pmatr.length - numNodes)  map (a => a.dropRight(a.length - numNodes)))
  }

  def assertMatches(a: Array[Array[Long]], b: Array[Array[Long]]): Unit = {
    assert(a.length == b.length)
    for (x <- a.indices) {
      assert(a(x).sameElements(b(x)))
    }
  }

  // A large graph with one edge.
  val singleEdgeGraph: Array[Array[Long]] = Array(Array(0, 50, 10))

  // A unidirectional bus graph.
  val directedBusGraph: Array[Array[Long]] = Array.tabulate(35) { i => Array(i, i+1, 10) }

  // An undirected bus graph.
  val undirectedBusGraph: Array[Array[Long]] = Array.tabulate(70) { i => if (i % 2 == 0) Array(i/2, i/2+1, 10) else Array(i/2+1, i/2, 10) }

  // A fully meshed graph, but only in one direction (low index => high index).
  val directedMeshGraph: Array[Array[Long]] = Array.tabulate[Array[Long]](70, 70) { (u, v) => Array[Long](u, v, 10) }.flatten

  // A fully meshed graph.
  val fullyMeshedGraph: Array[Array[Long]] =
    Array.tabulate[Array[Long]](70, 70) { (u, v) => Array[Long](u, v, 10) }.flatten ++ Array.tabulate[Array[Long]](70, 70) { (u, v) => Array[Long](v, u, 10) }.flatten

  // A full mesh with increasing edge weights.
    val unevenFullyMeshedGraph: Array[Array[Long]] =
      Array.tabulate[Array[Long]](70, 70) { (u, v) => Array[Long](u, v, u*v+10) }.flatten ++ Array.tabulate[Array[Long]](70, 70) { (u, v) => Array[Long](v, u, u*v+10) }.flatten

  // A directed tree going downwards from the root.
  val directedTreeGraph: Array[Array[Long]] = Array.tabulate(70) { i => Array(if (i != 0) ((if (i % 2 == 0) i else i+1) + 1)/2 - 1 else 0, i, 2*i)}

  // Unextended tree (i.e., one that perfectly maps to the processor grid)
  val unextendedDirectedBusGraph: Array[Array[Long]] = Array.tabulate(2 * 2 * 2 * 2 * 2) { i => Array(i, i+1, 10) }

  val SUCCESSOR = false
  val PREDECESSOR = true

  val graphs = Table("graphs",
    ("Undirected bus", undirectedBusGraph),
    ("Single edge", singleEdgeGraph),
    ("Directed bus", directedBusGraph),
    ("Directed mesh", directedMeshGraph),
    ("Full mesh", fullyMeshedGraph),
    ("Uneven full mesh", unevenFullyMeshedGraph),
    ("Directed tree", directedTreeGraph),
    ("Unextended directed bus", unextendedDirectedBusGraph)
  )

  val processorWidths = Table("processor widths", 2, 3, 4)

  val algorithms = Table("algorithms",
    ("Sequential Bellman-Ford", "src\\main\\parallang\\sequential_apsp\\bellman_ford_seq.plang", PREDECESSOR),
    ("Sequential Floyd-Warshall", "src\\main\\parallang\\sequential_apsp\\floyd_warshall_seq.plang", PREDECESSOR),
    ("Sequential MatMult", "src\\main\\parallang\\sequential_apsp\\matmult_seq.plang", PREDECESSOR),
    ("Naive Bellman-Ford", "src/main/parallang/apsp/bellman_ford_adj_list.plang", PREDECESSOR),
    ("Distance Vector", "src/main/parallang/apsp/distance_vector.plang", SUCCESSOR),
    ("Floyd-Warshall", "src/main/parallang/apsp/floyd_warshall.plang", PREDECESSOR),
    ("Fox-Otto", "src/main/parallang/apsp/foxs_general_apsp_edgelist.plang", PREDECESSOR),
//    ("Cache-Oblivious Fox-Otto", "src/main/parallang/apsp/foxs_general_cache_oblivious/foxs_general_apsp_cache_oblivious.plang", PREDECESSOR),
    ("Pipelined Cannon's", "src/main/parallang/apsp/cannons.plang", PREDECESSOR),
    ("Unpipelined Cannon's", "src/main/parallang/apsp/unpipelined_cannons.plang", PREDECESSOR),
//    ("Cache-Oblivious Pipelined Cannon's", "src/main/parallang/apsp/cannons_cache_oblivious/cannons_cache_oblivious.plang", PREDECESSOR)
  )


  for ((graphName, graph) <- graphs) {

    val (correctDistance, correctPredecessor, correctSuccessor) = floydWarshall(graph)

    val numNodes = numNodesFrom(graph)

    for ((algorithmName, path, pathMatrixType) <- algorithms)
      for (q <- processorWidths) {
        test(s"$graphName: $algorithmName run with $q x $q processors") {
          var distance: Array[Array[Long]] = Array.ofDim[Long](0, 0)
          var pathMatrix: Array[Array[Long]] = Array.ofDim[Long](0, 0)
          val (distance1, pathMatrix1) = runParallangProgram(path, graph, q, numNodes)
          distance = distance1
          pathMatrix = pathMatrix1
          assert(distance === correctDistance)
          assert(pathMatrix === (if (pathMatrixType == PREDECESSOR) correctPredecessor else correctSuccessor))
        }
      }
  }

}
