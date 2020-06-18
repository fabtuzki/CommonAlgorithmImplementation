package Graph

import scala.collection.mutable._

class DirectedGraphWithWeight(verticeC: Int) extends GraphWithWeight {
  override val vertice: Int = verticeC

  override def addEdge(source: Int, dest: Int, weight: Int): Unit = {
    try {
      graph(source).prepend((dest, weight))
      edgeCount += 1
    } catch {
      case e: ArrayIndexOutOfBoundsException => println("Exceed vertice count input in class!!!")
      case _ => println("other error: \n" + StackTraceElement)
    }
  }

  override def removeEdge(source: Int, dest: Int): Unit = {
    try {
      val node = graph(source).filter(x => x._1 == dest)
      graph(source) --= node
      edgeCount -= node.length
    } catch {
      case e: ArrayIndexOutOfBoundsException => println("Exceed vertice count input in class!!!")
      case _ => println("other error: \n" + StackTraceElement)
    }
  }

  def singleSourceShortestPath(source: Int): Array[Double] = {
    //implement dijkstra's algorithm
    //Init priority queue
    val queue = PriorityQueue[(Int, Double)]((source, 0))(Ordering.by(_._2))

    val dist = Array.fill[Double](vertice)(Double.PositiveInfinity)


    while (queue.nonEmpty) {
      val minDist = queue.dequeue()._1
      val adjEdge = graph(minDist)
      //calculate dist to adj edge
      adjEdge.foreach(x => {
        if (dist(minDist) + x._2 < dist(x._1)) {
          dist(x._1) = dist(minDist) + x._2
          // if (queue.exists(a => a._1 != x._1))
          queue.enqueue((x._1, dist(x._1)))
        }

      })
    }

    dist
  }


}
