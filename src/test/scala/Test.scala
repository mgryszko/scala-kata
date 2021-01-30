import cats.effect.IO
import org.scalatest.funsuite.AnyFunSuite

class Test extends AnyFunSuite {
  implicit def convertToIO[A](body: => A): IO[A] = IO(body)

  test("test is run") {
    val log: IO[Unit] = println("bla")
    val logs = for {
      _ <- log
      _ <- log
    } yield ()
    logs.unsafeRunSync()
    logs.unsafeRunSync()
  }
}
