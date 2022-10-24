package solver
import maze.{GameMap, GridLocation}
import org.scalatest._

class testing extends FunSuite{
  test("pathfinding"){
    val a = new GridLocation(0,1)
    val b= new GridLocation(0,5)
    val lst = PathFinding.findPath(a,b,GameMap.apply().tiles)
  }
}
