package Graph

import scala.collection.mutable._

trait GraphWithWeight {
  val vertice: Int
  val graph = Array.ofDim[ListBuffer[(Int, Int)]](vertice)
  var edgeCount = 0

  def addEdge(source: Int, dest: Int, weight: Int): Unit

  def removeEdge(source: Int, dest: Int): Unit

  def graphCreate(adjListEdge: Array[(Int, Int, Int)]): Array[List[(Int, Int)]] = {
    for (i <- adjListEdge) {
      addEdge(i._1, i._2, i._3)
      edgeCount += 1
    }
    val graphPrint = graph.map(x => x.toList)
    graphPrint
  }


}
