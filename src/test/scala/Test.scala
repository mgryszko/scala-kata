import cats._
import cats.data._
import cats.implicits._
import cats.syntax.all._
import org.scalatest.funspec.AnyFunSpec

object GameOfLife {
  sealed trait State
  object Alive extends State {
    override def toString: String = "A"
  }
  object Dead extends State {
    override def toString: String ="D"
  }

  case class Position(x: Int, y: Int) {
    lazy val north: Position = Position(x, y + 1)
    lazy val northWest: Position = Position(x + 1, y + 1)
    lazy val west: Position = Position(x + 1, y)
    lazy val southWest: Position = Position(x + 1, y - 1)
    lazy val south: Position = Position(x, y - 1)
    lazy val southEast: Position = Position(x - 1 , y - 1)
    lazy val east: Position = Position(x - 1, y)
    lazy val northEast: Position = Position(x - 1, y + 1)

    def alive: Cell = Cell(this, Alive)
    def dead: Cell = Cell(this, Dead)

    override def toString: String = s"($x,$y)"
  }

  case class Cell(pos: Position, state: State) {
    override def toString: String = s"$pos-$state"
  }

  type Population = Set[Position]

  def nextGen(population: Population): Population =
    meAndNeighbours(population).flatMap { cell =>
      val alive = aliveNeighbours(cell.pos, population)
      cell match {
        case Cell(pos, Alive) => if (alive == 2 || alive == 3) Some(pos) else None
        case Cell(pos, Dead) => if (alive == 3) Some(pos) else None
      }
    }

  private def aliveNeighbours(pos: Position, population: Population): Int =
    (neighbours(pos).toSet & population).size

  def meAndNeighbours(population: Population): Set[Cell] = {
    val allNeighbours = for {
      alivePosition <- population
      neighbour <- neighbours(alivePosition)
    } yield toCell(neighbour, population)
    (allNeighbours ++ population.map(_.alive)).toSet
  }

  private def neighbours(pos: Position): List[Position] =
    List(
      pos.north,
      pos.northWest,
      pos.west,
      pos.southWest,
      pos.south,
      pos.southEast,
      pos.east,
      pos.northEast,
    )
    
  private def toCell(pos: Position, population: Population): Cell =
    if (population.contains(pos)) pos.alive else pos.dead
}

class Test extends AnyFunSpec {
  import GameOfLife._

  implicit def toPosition(p: (Int, Int)): Position = Position(p._1, p._2)

  describe("blinker") {
    val verticalBlinker: Population = Set((0, 0), (0, 1), (0, 2))
    val horizontalBlinker: Population = Set((-1, 1), (0, 1), (1, 1))

    it("first generation") {
      assert(nextGen(verticalBlinker) === horizontalBlinker)
    }

    it("second generation") {
      assert(nextGen(nextGen(verticalBlinker)) === verticalBlinker)
    }

    it("third generation") {
      assert(nextGen(nextGen(nextGen(verticalBlinker))) === horizontalBlinker)
    }
  }

  describe("block") {
    val block: Population = Set((0, 0), (1, 0), (0, 1), (1, 1))

    it("two generations") {
      assert(nextGen(block) === block)
      assert(nextGen(nextGen(block)) === block)
    }
  }

  describe("glider") {
    val glider: Population = Set((0, 0), (1, 0), (2, 0), (2, 1), (1, 2))
    val gliderFirstGen: Population = Set((0, 1), (1, 0), (1, -1), (2, 0), (2, 1))
    val gliderSecondGen: Population = Set((0, 0), (1, -1), (2, -1), (2, 0), (2, 1))
    val gliderThirdGen: Population = Set((1, -1), (2, -1), (2, 0), (3, 0), (1, 1))
    val gliderFirstPeriod: Population = glider.map { case Position(x, y) => Position(x + 1, y - 1) }

    it("first generation") {
      assert(nextGen(glider) === gliderFirstGen)
    }

    it("second generation") {
      assert(nextGen(nextGen(glider)) === gliderSecondGen)
    }

    it("third generation") {
      assert(nextGen(nextGen(nextGen(glider))) === gliderThirdGen)
    }

    it("fourth generation") {
      assert(nextGen(nextGen(nextGen(nextGen(glider)))) === gliderFirstPeriod)
    }
  }
}
