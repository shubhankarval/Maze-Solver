package solver


class Graph[A] {

  var nodes: Map[List[Int], A] = Map()
  var adjacencyList: Map[List[Int], List[List[Int]]] = Map()


  def addNode(index: List[Int], a: A): Unit = {
    nodes += index -> a
    adjacencyList += index -> List()
  }

  def addEdge(index1: List[Int], index2: List[Int]): Unit = {
    adjacencyList += index1 -> (index2 :: adjacencyList(index1))
    adjacencyList += index2 -> (index1 :: adjacencyList(index2))
  }


  def areConnected(index1: List[Int], index2: List[Int]): Boolean = {
    // TODO: Return true if the two nodes are connected by a path, false otherwise
    var explored: Set[List[Int]] = Set(index1)
    val toExplore: Queue[List[Int]] = new Queue()
    toExplore.enqueue(index1)

    while (!toExplore.empty()) {
      val nodeToExplore = toExplore.dequeue()
      for (node <- adjacencyList(nodeToExplore)) {
        if (!explored.contains(node)) {
          toExplore.enqueue(node)
          explored = explored + node
        }
      }
    }
    if (explored.contains(index2)) {
      return true
    }
    false
  }


  def distance2(i1: List[Int], i2: List[Int]): Int = {
    var explored: Set[List[Int]] = Set(i1)
    var distance: Map[List[Int], List[Int]] = Map()
    distance += i1 -> List()
    val toExplore: Queue[List[Int]] = new Queue()
    toExplore.enqueue(i1)

    while (!toExplore.empty()) {
      val nodeToExplore = toExplore.dequeue()
      for (node <- adjacencyList(nodeToExplore)) {
        if (!explored.contains(node)) {
          //println("exploring: " + nodes(node))
          distance += node -> nodeToExplore
          toExplore.enqueue(node)
          explored = explored + node

        }
      }
    }
    var temp = i2
    var i = 0
    while (i <= distance.keys.toList.length) {
      i += 1
      if (distance(temp) == i1) {
        return i
      }
      else {
        temp = distance(temp)
      }
    }
    i
  }
}
