package Graph

import scala.collection.mutable._

class DirectedGraph(verticeC: Int) extends Graph {
  override val vertice: Int = verticeC
  val graph = Array.ofDim[ListBuffer[Int]](vertice)
  var edgeCount = 0

  override def addEdge(source: Int, dest: Int): Unit = {
    try {
      graph(source).prepend(dest)
      edgeCount += 1
    } catch {
      case e: ArrayIndexOutOfBoundsException => println("Exceed vertice count input in class!!!")
      case _ => println("other error: \n" + StackTraceElement)
    }


  }

  override def removeEdge(source: Int, dest: Int): Unit = {
    try {
      graph(source) -= dest
      edgeCount -= 1
    } catch {
      case e: ArrayIndexOutOfBoundsException => println("Vertex doesn't exists!!!")
      case _ => println("other error: \n" + StackTraceElement)
    }

  }

  val indegreeArr = Array.ofDim[Int](vertice)

  //leave this array outside for reuse later
  def topoSort(): Array[Int] = {
    //implement Kahn’s algorithm for Topological Sorting
    var label = 0
    val toposortArr = Array.ofDim[Int](vertice)
    //compute in-degree for each node O(v+e)

    for (i <- 0 until vertice) {
      for (j <- 0 until graph(i).length) {
        indegreeArr(j) += 1
      }
    }

    //pick all indegree = 0 to a queue
    val queue = new Queue[Int]
    indegreeArr.zipWithIndex.foreach(x => if (x._1 == 0) queue.enqueue(x._2))
    //loop
    while (queue.nonEmpty) {
      val next = queue.head
      toposortArr(next) = label
      queue.dequeue()
      label += 1
      val adjNode = graph(next)
      adjNode.foreach(x => {
        indegreeArr(x) -= 1
        if (indegreeArr(x) == 0) {
          queue.enqueue(x)
        }
      })

    }
    toposortArr
  }


}
