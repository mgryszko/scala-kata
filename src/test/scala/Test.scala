import cats._
import cats.data._
import cats.implicits._
import cats.syntax.all._
import org.scalatest.funsuite.AnyFunSuite

object GameOfLife {
  sealed trait Cell
  object Alive extends Cell
  object Dead extends Cell

  def step(cell: Cell, aliveNeighbours: Int): Cell = Dead
}

class Test extends AnyFunSuite {
  import GameOfLife._

  test("single cell with 0 alive neighbours") {
    assert(step(Alive, 0) === Dead)
  }
}
