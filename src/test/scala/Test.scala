import org.scalatest.funspec.AnyFunSpec

import scala.collection.mutable.ListBuffer

class Test extends AnyFunSpec {
  describe("command processing") {
    def fixture = new {
      val commands = ListBuffer[Command]()
      val commandToMovement: Command => MarsRover => MarsRover = { command =>
        commands += command
        rover => rover.copy(position = rover.position.up)
      }
      val move: (List[Command], MarsRover) => MarsRover = MarsRover.move(commandToMovement)(_, _)
    }

    it("one command") {
      val f = fixture
      val rover = f.move(List(Forward), MarsRover(Position(0, 0), North))
      assert(rover == MarsRover(Position(0, 1), North))
      assert(f.commands == List(Forward))
    }

    it("two commands") {
      val f = fixture
      val rover = f.move(List(Forward, Backward), MarsRover(Position(0, 0), North))
      assert(rover == MarsRover(Position(0, 2), North))
      assert(f.commands == List(Forward, Backward))
    }

    it("three commands") {
      val f = fixture
      val rover = f.move(List(Forward, Forward, Backward), MarsRover(Position(0, 0), North))
      assert(rover == MarsRover(Position(0, 3), North))
      assert(f.commands == List(Forward, Forward, Backward))
    }
  }

  describe("rover movements") {
    import MarsRover._

    it("moves forward") {
      assert(moveForward(MarsRover(Position(0, 0), North)) == MarsRover(Position(0, 1), North))
      assert(moveForward(MarsRover(Position(0, 0), East)) == MarsRover(Position(1, 0), East))
      assert(moveForward(MarsRover(Position(0, 0), South)) == MarsRover(Position(0, -1), South))
      assert(moveForward(MarsRover(Position(0, 0), West)) == MarsRover(Position(-1, 0), West))
    }

    it("moves backward") {
      assert(moveBackward(MarsRover(Position(0, 0), North)) == MarsRover(Position(0, -1), North))
      assert(moveBackward(MarsRover(Position(0, 0), East)) == MarsRover(Position(-1, 0), East))
      assert(moveBackward(MarsRover(Position(0, 0), South)) == MarsRover(Position(0, 1), South))
      assert(moveBackward(MarsRover(Position(0, 0), West)) == MarsRover(Position(1, 0), West))
    }

    it("turns right") {
      assert(turnRight(MarsRover(Position(0, 0), North)) == MarsRover(Position(0, 0), East))
      assert(turnRight(MarsRover(Position(0, 0), East)) == MarsRover(Position(0, 0), South))
      assert(turnRight(MarsRover(Position(0, 0), South)) == MarsRover(Position(0, 0), West))
      assert(turnRight(MarsRover(Position(0, 0), West)) == MarsRover(Position(0, 0), North))
    }

    it("turns left") {
      assert(turnLeft(MarsRover(Position(0, 0), North)) == MarsRover(Position(0, 0), West))
      assert(turnLeft(MarsRover(Position(0, 0), East)) == MarsRover(Position(0, 0), North))
      assert(turnLeft(MarsRover(Position(0, 0), South)) == MarsRover(Position(0, 0), East))
      assert(turnLeft(MarsRover(Position(0, 0), West)) == MarsRover(Position(0, 0), South))
    }
  }
}

object MarsRover {
  def move(commandToMovement: Command => MarsRover => MarsRover)(commands: List[Command], rover: MarsRover): MarsRover = {
    commands.foldLeft(rover) { (rover, command) => commandToMovement(command)(rover) }
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
case object Counterclockwise extends Command
case object Clockwise extends Command

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
