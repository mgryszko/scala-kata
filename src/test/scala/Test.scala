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

  test("single backward command") {
    assert(move(Backward, MarsRover(Position(0, 0), North)) == MarsRover(Position(0, -1), North))
    assert(move(Backward, MarsRover(Position(0, 0), East)) == MarsRover(Position(-1, 0), East))
    assert(move(Backward, MarsRover(Position(0, 0), South)) == MarsRover(Position(0, 1), South))
    assert(move(Backward, MarsRover(Position(0, 0), West)) == MarsRover(Position(1, 0), West))
  }

  test("single turn right command") {
    assert(move(Right, MarsRover(Position(0, 0), North)) == MarsRover(Position(0, 0), East))
    assert(move(Right, MarsRover(Position(0, 0), East)) == MarsRover(Position(0, 0), South))
    assert(move(Right, MarsRover(Position(0, 0), South)) == MarsRover(Position(0, 0), West))
    assert(move(Right, MarsRover(Position(0, 0), West)) == MarsRover(Position(0, 0), North))
  }

  test("single turn left command") {
    assert(move(Left, MarsRover(Position(0, 0), North)) == MarsRover(Position(0, 0), West))
    assert(move(Left, MarsRover(Position(0, 0), East)) == MarsRover(Position(0, 0), North))
    assert(move(Left, MarsRover(Position(0, 0), South)) == MarsRover(Position(0, 0), East))
    assert(move(Left, MarsRover(Position(0, 0), West)) == MarsRover(Position(0, 0), South))
  }
}

object MarsRover {
  def move(command: Command, rover: MarsRover): MarsRover = command match {
    case Forward => moveForward(rover)
    case Backward => moveBackward(rover)
    case Left => turnLeft(rover)
    case Right => turnRight(rover)
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

sealed trait Command
case object Forward extends Command
case object Backward extends Command
case object Left extends Command
case object Right extends Command

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
