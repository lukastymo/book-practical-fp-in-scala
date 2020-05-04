package ch02

import cats.effect.{ IO, Sync }
import cats.implicits._

object Example03_EncapsulatingState extends App {

  trait Counter[F[_]] {
    def incr: F[Unit]
    def get: F[Int]
  }

  // Define interpreter
  import cats.effect.concurrent.Ref

  // private - state shall not leak
  class LiveCounter[F[_]] private (ref: Ref[F, Int]) extends Counter[F] {
    def incr: F[Unit] = ref.update(_ + 1)

    def get: F[Int] = ref.get
  }

  object LiveCounter {
    def make[F[_]: Sync]: F[Counter[F]] =
      Ref.of[F, Int](0).map(new LiveCounter(_))
  }

  def program(counter: Counter[IO]): IO[Unit] =
    counter.incr *> counter.incr

  (for {
    counter <- LiveCounter.make[IO]
    _       <- program(counter)
    result  <- counter.get
  } yield println(result))
    .unsafeRunSync()
}
