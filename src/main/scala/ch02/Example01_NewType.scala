package ch02

import cats.effect.IO
import cats.implicits._
import domain._

object Example01_NewType extends App {

  def lookup(name: String, email: String): IO[Option[User]] = IO.pure(User(name, email).some)

  // Problem 1
  lookup("aaa@aaa", "aaa")
  lookup("aaa", "aaa@aaa")
  lookup("", "")

  // Solution 1
  case class Username(value: String) extends AnyVal
  case class Email(value: String) extends AnyVal

  def lookup1(name: Username, email: Email) = IO.pure(User(name.value, email.value).some)

  // Problem 1.1
  lookup1(Username("aaa@aaa"), Email("aaa"))

  // Solution 2

  case class Username2 private (value: String) extends AnyVal
  case class Email2 private (value: String) extends AnyVal

  def lookup2(name: Username2, email: Email2) = IO.pure(User(name.value, email.value).some)

  def mkUsername(value: String): Option[Username2] =
    if (value.nonEmpty) Username2(value).some else none[Username2]

  def mkEmail(value: String): Option[Email2] =
    if (value.contains("@")) Email2(value).some else none[Email2]

  val result1 = (mkUsername("aaa"), mkEmail("aaa@aaa")).mapN {
    case (username, email) => lookup2(username, email)
  }

  val result2 = (mkUsername("aaa@aaa"), mkEmail("aaa")).mapN {
    case (username, email) => lookup2(username, email)
  }

  println(result1.isDefined)
  println(result2.isDefined)

  // Problem 2.1
  val result3 = (mkUsername("aaa"), mkEmail("aaa@aaa")).mapN {
    case (username, email) => lookup2(username.copy(value = ""), email)
  }
  println(result3.isDefined)

  // Solution 3
  sealed abstract class Username3(val value: String)
  sealed abstract class Email3(val value: String)

  def mkUsername3(value: String): Option[Username3] =
    if (value.nonEmpty) (new Username3(value) {}).some else none[Username3]

  def mkEmail3(value: String): Option[Email3] =
    if (value.contains("@")) (new Email3(value) {}).some else none[Email3]

  println(mkUsername3("aaa").map(_.value).isDefined)
  println(mkEmail3("aaa@aaa").map(_.value).isDefined)

  // but
  // Username3("qwe") doesn't compile
  // and boilerplate code
  // how to deal with boilercode? Macros

  // Solution 4 (Newtypes will eventually be replaced by opaque types in dotty)
  import io.estatico.newtype.macros._

  @newtype case class Username4(value: String)
  @newtype case class Email4(value: String)

  // Problem 4.1
  println(Email4("foo"))

  // Solution 5

  @newtype case class Email5 private (value: String)

  object Email5 {
    def fromString(value: String): Option[Email5] =
      if (value.contains("@")) Email5(value).some else none[Email5]

  }

  Email5("foo") // no! it does compile
  println(Email5.fromString("foo").isDefined)
  println(Email5.fromString("foo@foo").isDefined)

  // No idea how it should look like, newtype it seems to be pretty useless when it comes to validation

}
