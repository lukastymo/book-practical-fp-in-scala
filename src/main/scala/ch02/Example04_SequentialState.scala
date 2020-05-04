package ch02

import cats.data.State

object Example04_SequentialState extends App {

  val nextInt: State[Int, Int] =
    State(s => (s + 1, s * 2))

  def seq: State[Int, Int] =
    for {
      n1 <- nextInt
      n2 <- nextInt
      n3 <- nextInt
    } yield n1 + n2 + n3

  println(seq.runA(0).value)
}
