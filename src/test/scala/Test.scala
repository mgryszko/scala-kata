import MarsRover.MoveResult
import org.scalatest.funspec.AnyFunSpec

import scala.collection.mutable.ListBuffer

class Test extends AnyFunSpec {
  describe("command processing") {
    describe("without obstacles") {
      import MarsRover.Moved

      def fixture = new {
        val commands: ListBuffer[Command] = ListBuffer[Command]()
        private val commandToMovement: Command => MarsRover => MoveResult = { command =>
          commands += command
          rover => Moved(rover.copy(position = rover.position.up))
        }
        val move: (List[Command], MarsRover) => MoveResult = MarsRover.move(commandToMovement)(_, _)
      }

      it("one command") {
        val f = fixture
        val rover = f.move(List(Forward), MarsRover(Position(0, 0), North))
        assert(rover == Moved(MarsRover(Position(0, 1), North)))
        assert(f.commands == List(Forward))
      }

      it("two commands") {
        val f = fixture
        val rover = f.move(List(Forward, Backward), MarsRover(Position(0, 0), North))
        assert(rover == Moved(MarsRover(Position(0, 2), North)))
        assert(f.commands == List(Forward, Backward))
      }

      it("three commands") {
        val f = fixture
        val rover = f.move(List(Forward, Forward, Backward), MarsRover(Position(0, 0), North))
        assert(rover == Moved(MarsRover(Position(0, 3), North)))
        assert(f.commands == List(Forward, Forward, Backward))
      }
    }

    describe("with obstacle") {
      import MarsRover._

      it("obstacle on the way") {
        val commands = ListBuffer[Command]()
        val commandToMovement: Command => MarsRover => MoveResult = { command =>
          commands += command
          command match {
            case Clockwise => rover => ObstacleDetected(rover)
            case _ => rover => Moved(rover.copy(position = rover.position.up))
          }
        }

        val rover = move(commandToMovement)(List(Forward, Clockwise, Forward), MarsRover(Position(0, 0), North))

        assert(rover == ObstacleDetected(MarsRover(Position(0, 1), North)))
        assert(commands == List(Forward, Clockwise))
      }
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
  type MoveResult = Either[MarsRover, MarsRover]
  type ObstacleDetected = Left[MarsRover, MarsRover]
  type Moved = Right[MarsRover, MarsRover]

  object ObstacleDetected {
    def apply(rover: MarsRover) = Left(rover)
  }

  object Moved {
    def apply(rover: MarsRover) = Right(rover)
  }

  def move(commandToMovement: Command => MarsRover => MoveResult)(commands: List[Command], rover: MarsRover): MoveResult = {
    commands.foldLeft(Right(rover): MoveResult) { (rover, command) =>
      rover.flatMap(commandToMovement(command)(_))
    }
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
