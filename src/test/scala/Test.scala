import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite {
  import MarsRover.moveForward

  test("move forward") {
    assert(moveForward(MarsRover((0, 0), North)) == MarsRover((0, 1), North))
  }
}

object MarsRover {
  def moveForward(rover: MarsRover): MarsRover = MarsRover((0, 1), North)
}

case class MarsRover(position: (Int, Int), direction: Direction)

sealed class Direction
case object North extends Direction
case object East extends Direction
case object South extends Direction
case object West extends Direction
