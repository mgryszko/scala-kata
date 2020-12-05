import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite {
  import MarsRover._

  test("move forward") {
    assert(moveForward(MarsRover(Position(0, 0), North)) == MarsRover(Position(0, 1), North))
    assert(moveForward(MarsRover(Position(0, 0), East)) == MarsRover(Position(1, 0), East))
    assert(moveForward(MarsRover(Position(0, 0), South)) == MarsRover(Position(0, -1), South))
    assert(moveForward(MarsRover(Position(0, 0), West)) == MarsRover(Position(-1, 0), West))
  }

  test("move backward") {
    assert(moveBackward(MarsRover(Position(0, 0), North)) == MarsRover(Position(0, -1), North))
    assert(moveBackward(MarsRover(Position(0, 0), East)) == MarsRover(Position(-1, 0), East))
    assert(moveBackward(MarsRover(Position(0, 0), South)) == MarsRover(Position(0, 1), South))
    assert(moveBackward(MarsRover(Position(0, 0), West)) == MarsRover(Position(1, 0), West))
  }
}

object MarsRover {
  def moveForward(rover: MarsRover): MarsRover = rover.direction match {
    case North => rover.copy(position = rover.position.up)
    case East => rover.copy(position = rover.position.right)
    case South => rover.copy(position = rover.position.down)
    case West => rover.copy(position = rover.position.left)
  }

  def moveBackward(rover: MarsRover): MarsRover = rover.direction match {
    case North => rover.copy(position = rover.position.down)
    case East => rover.copy(position = rover.position.left)
    case South => rover.copy(position = rover.position.up)
    case West => rover.copy(position = rover.position.right)
  }
}

case class MarsRover(position: Position, direction: Direction)

case class Position(x: Int, y: Int) {
  def up: Position = Position(x, y + 1)
  def right: Position = Position(x + 1, y)
  def down: Position = Position(x, y - 1)
  def left: Position = Position(x - 1, y)
}

sealed class Direction
case object North extends Direction
case object East extends Direction
case object South extends Direction
case object West extends Direction
