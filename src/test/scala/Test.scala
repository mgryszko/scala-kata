import cats._
import cats.data._
import cats.implicits._
import cats.syntax.all._
import org.scalatest.funsuite.AnyFunSuite

object GameOfLife {
  sealed trait Cell
  object Alive extends Cell
  object Dead extends Cell

  def step(cell: Cell, aliveNeighbours: Int): Cell = cell match {
    case Alive => if (aliveNeighbours == 2 || aliveNeighbours == 3) Alive else Dead
    case Dead => if (aliveNeighbours == 3) Alive else Dead
  }
}

class Test extends AnyFunSuite {
  import GameOfLife._

  test("alive cell with 0 alive neighbours") {
    assert(step(Alive, 0) === Dead)
  }

  test("alive cell with 1 alive neighbours") {
    assert(step(Alive, 1) === Dead)
  }

  test("alive cell with 2 alive neighbours") {
    assert(step(Alive, 2) === Alive)
  }

  test("alive cell with 3 alive neighbours") {
    assert(step(Alive, 3) === Alive)
  }

  test("alive cell with 4 alive neighbours") {
    assert(step(Alive, 4) === Dead)
  }

  test("dead cell with 2 alive neighbours") {
    assert(step(Dead, 2) === Dead)
  }

  test("dead cell with 3 alive neighbours") {
    assert(step(Dead, 3) === Alive)
  }

  test("dead cell with 4 alive neighbours") {
    assert(step(Dead, 4) === Dead)
  }
}
