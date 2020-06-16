package Graph

import scala.collection.mutable._

trait Graph {
  val vertice: Int
  val graph: Array[ListBuffer[Int]]

  def addEdge(source: Int, dest: Int): Unit

  def graphCreate(adjListEdge: Array[(Int, Int)]): Array[List[Int]] = {
    for (i <- adjListEdge) {
      addEdge(i._1, i._2)
    }
    val graphPrint = graph.map(x => x.toList)
    graphPrint
  }

  def removeEdge(source: Int, dest: Int): Unit

  def bfs(source: Int, dest: Int): Boolean = {
    val queue = new Queue[Int]
    queue.enqueue(source)
    val visited = Array.ofDim[Int](vertice)
    while (visited(dest) == 0 || queue.nonEmpty) {
      val next = queue.head
      queue.dequeue()
      val adjNext = graph(next)
      visited(next) = 1
      adjNext.foreach(x => if (visited(x) == 0) queue.enqueue(x))
    }
    if (visited(dest) == 1) {
      true
    } else false
  }

  def dfs(source: Int, dest: Int): Boolean = {
    val stack = new Stack[Int]
    stack.push(source)
    val visited = Array.ofDim[Int](vertice)
    while (visited(dest) == 0 || stack.isEmpty) {
      val next = stack.head
      stack.pop()
      val adjNext = graph(next)
      visited(next) = 1
      adjNext.foreach(x => if (visited(x) == 0) stack.push(x))
    }
    if (visited(dest) == 1) {
      true
    } else false
  }

  def shortestPath(source: Int): Array[Int] = {
    val distArr = Array.fill[Int](vertice)(-1)
    val queue = new Queue[Int]
    val visited = Array.ofDim[Int](vertice)
    distArr(source) = 0
    queue.enqueue(source)
    visited(source) = 1
    while (queue.nonEmpty) {
      val adjList = graph(queue.head)
      adjList.foreach(x => if (visited(x) == 0) {
        distArr(queue.head) match {
          case _ > 0 => distArr(x) = distArr(queue.head) + 1
          case _ < 0 => throw new Exception("Error in tranversing node!!! x = " + queue.head)
        }
        queue.dequeue()
        queue.enqueue(x)
      })
    }
    distArr
  }


}
