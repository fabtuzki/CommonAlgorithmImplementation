package Graph

import DS.DisjointSet

import scala.collection.mutable._

class UndirectedGraphWithWeight(verticeC: Int) extends GraphWithWeight {
  override val vertice: Int = verticeC

  override def addEdge(source: Int, dest: Int, weight: Int): Unit = {
    try {
      graph(source).prepend((dest, weight))
      graph(dest).prepend((source, weight))
      edgeCount += 1
    } catch {
      case e: ArrayIndexOutOfBoundsException => println("Exceed vertice count input in class!!!")
      case _ => println("other error: \n" + StackTraceElement)
    }
  }

  override def removeEdge(source: Int, dest: Int): Unit = {
    try {
      val nodeS = graph(source).filter(x => x._1 == dest)
      val nodeD = graph(dest).filter(x => x._1 == source)

      graph(source) --= nodeS
      graph(dest) --= nodeD

      edgeCount -= (nodeS.length + nodeD.length)
    } catch {
      case e: ArrayIndexOutOfBoundsException => println("Exceed vertice count input in class!!!")
      case _ => println("other error: \n" + StackTraceElement)
    }
  }


  def minimumSpanningTreePrim(source: Int): Either[Array[Int], Unit] = {
    if (graph.exists(x => x.isEmpty)) Right(println("Unconnected Graph!!!!")) else {
      val visited = Array.fill[Boolean](vertice)(false)
      val queue = PriorityQueue[(Double, Int)]((0, source))(Ordering.by(_._1))
      val keyValue = Array.fill[Double](vertice)(Double.PositiveInfinity)
      val tree = new ListBuffer[Int]
      tree.append(source)
      while (queue.nonEmpty) {
        val u = queue.dequeue()
        visited(u._2) = true
        tree.append(u._2)
        graph(u._2).foreach(v => {
          if (visited(v._2) == false && keyValue(v._2) > graph(u._2).filter(x => x._1 == v._2).head._2) {
            keyValue(v._2) = graph(u._2).filter(x => x._1 == v._2).head._2
            queue.enqueue((keyValue(v._2), v._2))
            tree.append(v._2)
          }
        })
      }
      Right(tree.toArray)
    }
  }


  def minimumSpanningTreeKruskal(edgeList: Array[(Int, Int, Double)], source: Int): Either[Array[(Int, Int, Double)], Unit] = {
    if (graph.exists(x => x.isEmpty)) Right(println("Unconnected Graph!!!!")) else {
      val tree = Array.ofDim[(Int, Int, Double)](vertice)
      var e = 0

      val edgeListSorted = edgeList.sortBy(x => x._3)
      val disjSet = new DisjointSet(vertice)
      for (i <- 0 until vertice) {
        disjSet.subsetArr(i).parent = i
        disjSet.subsetArr(i).rank = 0
      }
      var i = 0
      while (e < vertice - 1) {
        val next = edgeListSorted(i)
        i += 1
        val x = disjSet.find(next._1)
        val y = disjSet.find(next._2)
        if (x != y) {
          tree(e) = next
          e += 1
          disjSet.union(x, y)
        }

      }

      Left(tree)
    }
  }

}
