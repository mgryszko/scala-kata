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

  test("turn right") {
    assert(turnRight(MarsRover(Position(0, 0), North)) == MarsRover(Position(0, 0), East))
    assert(turnRight(MarsRover(Position(0, 0), East)) == MarsRover(Position(0, 0), South))
    assert(turnRight(MarsRover(Position(0, 0), South)) == MarsRover(Position(0, 0), West))
    assert(turnRight(MarsRover(Position(0, 0), West)) == MarsRover(Position(0, 0), North))
  }

  test("turn left") {
    assert(turnLeft(MarsRover(Position(0, 0), North)) == MarsRover(Position(0, 0), West))
    assert(turnLeft(MarsRover(Position(0, 0), East)) == MarsRover(Position(0, 0), North))
    assert(turnLeft(MarsRover(Position(0, 0), South)) == MarsRover(Position(0, 0), East))
    assert(turnLeft(MarsRover(Position(0, 0), West)) == MarsRover(Position(0, 0), South))
  }

  test("single forward command") {
    assert(move(Forward, MarsRover(Position(0, 0), North)) == MarsRover(Position(0, 1), North))
    assert(move(Forward, MarsRover(Position(0, 0), East)) == MarsRover(Position(1, 0), East))
    assert(move(Forward, MarsRover(Position(0, 0), South)) == MarsRover(Position(0, -1), South))
    assert(move(Forward, MarsRover(Position(0, 0), West)) == MarsRover(Position(-1, 0), West))
  }
}

object MarsRover {
  def move(command: Command, rover: MarsRover): MarsRover = command match {
    case Forward => moveForward(rover)
  }

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

  def turnRight(rover: MarsRover): MarsRover = rover.copy(direction = rover.direction.right)

  def turnLeft(rover: MarsRover): MarsRover = rover.copy(direction = rover.direction.left)
}

case class MarsRover(position: Position, direction: Direction)

case class Position(x: Int, y: Int) {
  def up: Position = Position(x, y + 1)
  def right: Position = Position(x + 1, y)
  def down: Position = Position(x, y - 1)
  def left: Position = Position(x - 1, y)
}

sealed trait Direction {
  def right: Direction
  def left: Direction
}
case object North extends Direction {
  override def right: Direction = East

  override def left: Direction = West
}
case object East extends Direction {
  override def right: Direction = South

  override def left: Direction = North
}
case object South extends Direction {
  override def right: Direction = West

  override def left: Direction = East
}
case object West extends Direction {
  override def right: Direction = North

  override def left: Direction = South
}

sealed trait Command
case object Forward extends Command