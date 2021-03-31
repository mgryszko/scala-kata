import cats._
import cats.data._
import cats.implicits._
import cats.syntax.all._
import org.scalatest.funspec.AnyFunSpec

object GameOfLifeComonad {
  sealed trait Cell
  object Alive extends Cell {
    override def toString = s"A"
  }
  object Dead extends Cell {
    override def toString = s"D"
  }
  type Population = Map[Position, Cell]

  type Board[A] = Store[Position, A]

  case class Position(x: Int, y: Int) {
    lazy val north: Position = Position(x, y + 1)
    lazy val northWest: Position = Position(x + 1, y + 1)
    lazy val west: Position = Position(x + 1, y)
    lazy val southWest: Position = Position(x + 1, y - 1)
    lazy val south: Position = Position(x, y - 1)
    lazy val southEast: Position = Position(x - 1, y - 1)
    lazy val east: Position = Position(x - 1, y)
    lazy val northEast: Position = Position(x - 1, y + 1)

    def neighbours: List[Position] =
      List(north, northWest, west, southWest, south, southEast, east, northEast)

    override def toString: String = s"($x,$y)"
  }

  def nextGen(board: Board[Cell])(implicit W: Comonad[Board]): Board[Cell] =
    W.coflatMap(board) { b =>
      val alive = aliveNeighbours(b)
      if ((b.extract == Alive && alive == 2) || alive == 3) Alive else Dead
    }

  private def aliveNeighbours(board: Board[Cell]): Int =
    board.experiment(_.neighbours).count(_ == Alive)
}

class GameOfLifeComonadTest extends AnyFunSpec {
  import GameOfLifeComonad._

  type Coord = (Int, Int)

  describe("blinker") {
    val verticalBlinker: Population = toPopulation(List((0, 0), (0, 1), (0, 2)))
    val horizontalBlinker: Population = toPopulation(List((-1, 1), (0, 1), (1, 1)))

    it("first generation") {
      assert(toPopulation(nextGen(toBoard(verticalBlinker))) === horizontalBlinker)
    }

    it("second generation") {
      assert(toPopulation(nextGen(nextGen(toBoard(verticalBlinker)))) === verticalBlinker)
    }

    it("third generation") {
      assert(toPopulation(nextGen(nextGen(nextGen(toBoard(verticalBlinker))))) === horizontalBlinker)
    }
  }

  describe("block") {
    val block: Population = toPopulation(List((0, 0), (1, 0), (0, 1), (1, 1)))

    it("two generations") {
      assert(toPopulation(nextGen(toBoard(block))) === block)
      assert(toPopulation(nextGen(nextGen(toBoard(block)))) === block)
    }
  }

  describe("glider") {
    val glider: Population = toPopulation(List((0, 0), (1, 0), (2, 0), (2, 1), (1, 2)))
    val gliderFirstGen: Population = toPopulation(List((0, 1), (1, 0), (1, -1), (2, 0), (2, 1)))
    val gliderSecondGen: Population = toPopulation(List((0, 0), (1, -1), (2, -1), (2, 0), (2, 1)))
    val gliderThirdGen: Population = toPopulation(List((1, -1), (2, -1), (2, 0), (3, 0), (1, 1)))
    val gliderFirstPeriod: Population = glider.map { case (Position(x, y), cell) => Position(x + 1, y - 1) -> cell }

    it("first generation") {
      assert(toPopulation(nextGen(toBoard(glider))) === gliderFirstGen)
    }

    it("second generation") {
      assert(toPopulation(nextGen(nextGen(toBoard(glider)))) === gliderSecondGen)
    }

    it("third generation") {
      assert(toPopulation(nextGen(nextGen(nextGen(toBoard(glider))))) === gliderThirdGen)
    }

    it("fourth generation") {
      assert(toPopulation(nextGen(nextGen(nextGen(nextGen(toBoard(glider)))))) === gliderFirstPeriod)
    }
  }

  def toBoard(population: Population): Board[Cell] = Store(pos => population.getOrElse(pos, Dead), population.head._1)

  def toPopulation(coords: List[Coord]): Population = coords.map(p => Position(p._1, p._2) -> Alive).toMap

  def toPopulation(board: Board[Cell]): Population = {
    val positions = for {
      x <- -5 to 5
      y <- -5 to 5
    } yield Position(x, y)
    positions.map(pos => pos -> board.peek(pos))
      .filter(_._2 == Alive)
      .toMap
  }
}
