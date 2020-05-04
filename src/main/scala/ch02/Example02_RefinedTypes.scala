package ch02

import domain._
import cats.implicits._

object Example02_RefinedTypes extends App {
  import eu.timepit.refined.api._
  import eu.timepit.refined.collection._
  import eu.timepit.refined.auto._
  import eu.timepit.refined.types.string.NonEmptyString

  def lookup(name: NonEmptyString): Option[User] =
    User(name.value, "").some

  println(lookup("aaa"))
//  println(lookup(""))

  type Username = String Refined Contains['g']

  def lookup2(name: Username): Option[User] =
    User(name.value, "").some

  println(lookup2("adsadg"))
//  println(lookup2("adsad"))
}
