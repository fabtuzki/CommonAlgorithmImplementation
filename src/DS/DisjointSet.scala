package DS

class DisjointSet(l: Int) {
  val subsetArr = Array.ofDim[subset](l)

  def find(i: Int): Int = {
    if (subsetArr(i).parent != i) {
      subsetArr(i).parent = find(subsetArr(i).parent)
    }
    subsetArr(i).parent
  }


  def union(x: Int, y: Int): Unit = {
    val xRoot = find(x)
    val yRoot = find(y)

    if (subsetArr(xRoot).rank < subsetArr(yRoot).rank)
      subsetArr(xRoot).parent = yRoot
    else if (subsetArr(xRoot).rank > subsetArr(yRoot).rank)
      subsetArr(yRoot).parent = xRoot
    else {
      subsetArr(yRoot).parent = xRoot
      subsetArr(xRoot).rank += 1
    }

  }


}

case class subset(var parent: Int, var rank: Int)