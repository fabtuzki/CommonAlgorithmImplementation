package Graph

import scala.collection.mutable._

class UndirectedGraph(verticeC: Int) extends Graph {
  override val vertice: Int = verticeC
  var edgeCount = 0
  val graph = Array.ofDim[ListBuffer[Int]](vertice)


  override def addEdge(source: Int, dest: Int): Unit = {
    try {
      graph(source).prepend(dest)
      graph(dest).prepend(source)
      edgeCount += 1
    } catch {
      case e: ArrayIndexOutOfBoundsException => println("Exceed vertice count input in class!!!")
      case _ => println("other error: \n" + StackTraceElement)
    }
  }

  override def removeEdge(source: Int, dest: Int): Unit = {
    try {

      graph(source) -= dest
      graph(dest) -= source
      edgeCount -= 1
    } catch {
      case e: ArrayIndexOutOfBoundsException => println("Vertex doesn't exists!!!")
      case _ => println("other error: \n" + StackTraceElement)
    }

  }

  def genericSearch(source: Int, dest: Int, bfsChoice: Boolean = true): Boolean = {
    if (bfsChoice == true) {
      bfs(source, dest)
    } else dfs(source, dest)
  }


  def connectedComponent(): (Array[Int], Int) = {
    val ccList, visited = Array.fill[Int](vertice)(0)
    var numCC = 0
    for (i <- 0 until graph.length) {
      if (visited(i) == 0) {
        numCC += 1
        val queue = new Queue[Int]
        queue.enqueue(i)
        while (queue.nonEmpty) {
          val next = queue.head
          queue.dequeue()
          val adjNext = graph(next)
          ccList(next) = numCC
          visited(next) = 1
          adjNext.foreach(x => if (visited(x) == 0) queue.enqueue(x))
        }
      }
    }

    (ccList, numCC)
  }

}
