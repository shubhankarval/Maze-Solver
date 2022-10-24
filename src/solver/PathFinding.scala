package solver

import akka.actor.CoordinatedShutdown.Phase
import maze.{GameMap, GridLocation, MapTile, PhysicsVector}


object PathFinding {

  def findPath(start: GridLocation, end: GridLocation, map: List[List[MapTile]]): List[GridLocation] = {
    if(!map(start.y)(start.x).passable || !map(end.y)(end.x).passable){
      return List[GridLocation]()
    }
    if(start.equals(end)){
      return List[GridLocation](start)
    }
    val graph=toGraph(map)
    var s = List(start.x, start.y)
    var e=List(end.x,end.y)
    var lst:List[GridLocation]=List()
    if(!graph.areConnected(s,e)){
      List[GridLocation]()
    }

    var explored: Set[List[Int]] = Set(s)
    var distance: Map[List[Int], List[Int]] = Map()
    distance += s -> s
    val toExplore: Queue[List[Int]] = new Queue()
    toExplore.enqueue(s)
    while (!toExplore.empty()) {
      val nodeToExplore = toExplore.dequeue()
      for (node <- graph.adjacencyList(nodeToExplore)) {
        if(!explored.contains(node)){
          distance += node -> nodeToExplore
          toExplore.enqueue(node)
          explored = explored + node
        }
      }
    }
    var temp=e
    while(distance.contains(temp)){
      if(distance(temp)==s){
        lst = new GridLocation(temp.head,temp(1)) :: lst
        lst=start::lst
        return lst
      }
      else{
        val g = new GridLocation(temp.head, temp(1))
        lst = g :: lst
        temp=distance(temp)
      }
    }
    lst
  }


  def getVelocity(path: List[GridLocation], currentLocation: PhysicsVector): PhysicsVector = {
    if(path.isEmpty) {
      new PhysicsVector(0.0,0.0)
    }
    else {

      if (path.last.x == currentLocation.x.toInt && path.last.y == currentLocation.y.toInt) {
        var a = new PhysicsVector(currentLocation.x.floor + 0.5, currentLocation.y.floor + 0.5)
        if (a.distance2d(currentLocation) <= 0.1) {
          return new PhysicsVector(0.0, 0.0)
        }
        else {
          a = new PhysicsVector(a.x - currentLocation.x, a.y - currentLocation.y).normal2d()
          a = new PhysicsVector(a.x * 5.0, a.y * 5.0)
          return a
        }
      }
      var centre = new PhysicsVector(0.0, 0.0)
      for (j <- 0 until path.length - 1) {
        if (path(j).x == currentLocation.x.toInt && path(j).y == currentLocation.y.toInt) {
          centre = new PhysicsVector(path(j + 1).x.floor + 0.5 - currentLocation.x, path(j + 1).y.floor + 0.5 - currentLocation.y)
        }
      }
      val vector = centre.normal2d()
      val ans = new PhysicsVector(vector.x * 5.0, vector.y * 5.0)
      ans
    }
  }

  def toGraph(map: List[List[MapTile]]):Graph[List[Int]]={
    val graph: Graph[List[Int]] = new Graph()

    for(lst <- map){
      for(ele <- lst){
        if(ele.passable){
          graph.addNode(List(lst.indexOf(ele),map.indexOf(lst)),List(lst.indexOf(ele),map.indexOf(lst)))
        }
      }
    }

    for(lst<-map){
      for(j<-0 until lst.length-1){
        if(lst(j).passable && lst(j+1).passable){
          graph.addEdge(List(lst.indexOf(lst(j)),map.indexOf(lst)),List(lst.indexOf(lst(j+1)),map.indexOf(lst)))
        }
      }
    }

    for(k <- 0 until map.length-1){
      for((x,y)<- (map(k) zip map(k+1))){
        if(x.passable && y.passable){
          graph.addEdge(List(map(k).indexOf(x),map.indexOf(map(k))),List(map(k+1).indexOf(y),map.indexOf(map(k+1))))
        }
      }
    }
    graph
  }

  def main(args: Array[String]): Unit = {
    val a=new PhysicsVector(2.72,15.71)
    var b=new PhysicsVector(2.5,14.5)
    b=new PhysicsVector(b.x-a.x,b.y-a.y)
    println(b.x*5.0,b.y*5.0)

  }

}
