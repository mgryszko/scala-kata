import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite {
  import MarsRover._

  test("move forward") {
    assert(moveForward(MarsRover((0, 0), North)) == MarsRover((0, 1), North))
    assert(moveForward(MarsRover((0, 0), East)) == MarsRover((1, 0), East))
    assert(moveForward(MarsRover((0, 0), South)) == MarsRover((0, -1), South))
    assert(moveForward(MarsRover((0, 0), West)) == MarsRover((-1, 0), West))
  }

  test("move backward") {
    assert(moveBackward(MarsRover((0, 0), North)) == MarsRover((0, -1), North))
    assert(moveBackward(MarsRover((0, 0), East)) == MarsRover((-1, 0), East))
    assert(moveBackward(MarsRover((0, 0), South)) == MarsRover((0, 1), South))
    assert(moveBackward(MarsRover((0, 0), West)) == MarsRover((1, 0), West))
  }
}

object MarsRover {
  def moveForward(rover: MarsRover): MarsRover = rover.direction match {
    case North => rover.copy(position = (rover.position._1, rover.position._2 + 1))
    case East =>rover.copy(position = (rover.position._1 + 1, rover.position._2))
    case South =>rover.copy(position = (rover.position._1, rover.position._2 - 1))
    case West =>rover.copy(position = (rover.position._1 - 1, rover.position._2))
  }

  def moveBackward(rover: MarsRover): MarsRover = rover.direction match {
    case North => rover.copy(position = (rover.position._1, rover.position._2 - 1))
    case East =>rover.copy(position = (rover.position._1 - 1, rover.position._2))
    case South =>rover.copy(position = (rover.position._1, rover.position._2 + 1))
    case West =>rover.copy(position = (rover.position._1 + 1, rover.position._2))
  }
}

case class MarsRover(position: (Int, Int), direction: Direction)

sealed class Direction
case object North extends Direction
case object East extends Direction
case object South extends Direction
case object West extends Direction
