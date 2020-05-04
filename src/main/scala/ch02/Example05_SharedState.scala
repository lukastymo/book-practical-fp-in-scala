package ch02

import cats.effect.concurrent.Semaphore
import cats.effect.{ ExitCode, IO, IOApp }
import cats.implicits._
import cats.effect._
import cats.effect.concurrent.Semaphore
import cats.effect.implicits._
import cats.implicits._
import scala.concurrent.duration._

object Example05_SharedState extends IOApp {
  def putStrLn[A](a: A): IO[Unit] = IO(println(a))

  def someExpensiveTask: IO[Unit] =
    IO.sleep(1.second) >>
        putStrLn("expensive task") >>
        someExpensiveTask

  def process1(sem: Semaphore[IO]): IO[Unit] =
    sem.withPermit(someExpensiveTask) >> p1(sem)

  def process2(sem: Semaphore[IO]): IO[Unit] =
    sem.withPermit(someExpensiveTask) >> process2(sem)

  def run(args: List[String]): IO[ExitCode] =
    Semaphore[IO](1).flatMap { sem =>
      process1(sem).start.void *>
        process2(sem).start.void
    } *> IO.never.as(ExitCode.Success)
}
