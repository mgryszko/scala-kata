import cats._
import cats.data._
import cats.implicits._
import cats.syntax.all._
import org.scalatest.funspec.AnyFunSpec

object GameOfLife {
  sealed trait State
  object Alive extends State
  object Dead extends State

  def step(cell: State, aliveNeighbours: Int): State = cell match {
    case Alive => if (aliveNeighbours == 2 || aliveNeighbours == 3) Alive else Dead
    case Dead => if (aliveNeighbours == 3) Alive else Dead
  }
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
}
