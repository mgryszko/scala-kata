import cats._
import cats.data._
import cats.implicits._
import cats.syntax.all._
import org.scalatest.funspec.AnyFunSpec

object GameOfLife {
  sealed trait State
  object Alive extends State
  object Dead extends State

  type Position = (Int, Int)

  def step(cell: State, aliveNeighbours: Int): State = cell match {
    case Alive => if (aliveNeighbours == 2 || aliveNeighbours == 3) Alive else Dead
    case Dead => if (aliveNeighbours == 3) Alive else Dead
  }

  def cellAt(board: List[Position], pos: Position): State =
    if (board.contains(pos)) Alive else Dead
}

class Test extends AnyFunSpec {
  import GameOfLife._

  describe("numeric alive neighbours") {
    it("alive cell with 0 alive neighbours") {
      assert(step(Alive, 0) === Dead)
    }

    it("alive cell with 1 alive neighbours") {
      assert(step(Alive, 1) === Dead)
    }

    it("alive cell with 2 alive neighbours") {
      assert(step(Alive, 2) === Alive)
    }

    it("alive cell with 3 alive neighbours") {
      assert(step(Alive, 3) === Alive)
    }

    it("alive cell with 4 alive neighbours") {
      assert(step(Alive, 4) === Dead)
    }

    it("dead cell with 2 alive neighbours") {
      assert(step(Dead, 2) === Dead)
    }

    it("dead cell with 3 alive neighbours") {
      assert(step(Dead, 3) === Alive)
    }

    it("dead cell with 4 alive neighbours") {
      assert(step(Dead, 4) === Dead)
    }
  }

  describe("board") {
    it("dead or alive cells") {
      val blinker = List((0, 0) ,(0, 1), (0, 2))

      assert(cellAt(blinker, (0, 0)) === Alive)
      assert(cellAt(blinker, (0, 1)) === Alive)
      assert(cellAt(blinker, (0, 2)) === Alive)

      assert(cellAt(blinker, (-1, 0)) === Dead)
      assert(cellAt(blinker, (-1, -1)) === Dead)
      assert(cellAt(blinker, (0, -1)) === Dead)
      assert(cellAt(blinker, (1, -1)) === Dead)
      assert(cellAt(blinker, (1, 0)) === Dead)
      assert(cellAt(blinker, (1, 1)) === Dead)

      assert(cellAt(blinker, (-1, 1)) === Dead)
      assert(cellAt(blinker, (-1, 2)) === Dead)
      assert(cellAt(blinker, (-1, 3)) === Dead)
      assert(cellAt(blinker, (0, 3)) === Dead)
      assert(cellAt(blinker, (1, 3)) === Dead)
      assert(cellAt(blinker, (1, 2)) === Dead)
    }
  }
}
