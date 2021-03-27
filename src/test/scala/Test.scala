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

  implicit def toPosition(p: (Int, Int)): Position = Position(p._1, p._2)

  case class Cell(pos: Position, state: State) {
    override def toString: String = s"$pos-$state"
  }

  type Population = List[Position]

  def step(cell: State, aliveNeighbours: Int): State = cell match {
    case Alive => if (aliveNeighbours == 2 || aliveNeighbours == 3) Alive else Dead
    case Dead => if (aliveNeighbours == 3) Alive else Dead
  }

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
    val blinker: List[Position] = List((0, 0) ,(0, 1), (0, 2))

    it("population and its neighbours") {
      assert(meAndNeighbours(blinker) === Set(
        Cell((0, 0), Alive),
        Cell((0, 1), Alive),
        Cell((0, 2), Alive),
        Cell((0, 3), Dead),
        Cell((1, 3), Dead),
        Cell((1, 2), Dead),
        Cell((1, 1), Dead),
        Cell((1, 0), Dead),
        Cell((1, -1), Dead),
        Cell((-1, -1), Dead),
        Cell((0, -1), Dead),
        Cell((-1, 0), Dead),
        Cell((-1, 1), Dead),
        Cell((-1, 2), Dead),
        Cell((-1, 3), Dead),
      ))
    }
  }
}
