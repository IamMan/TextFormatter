package com.nikita.twitter

//Just for convenient. In real project you need to be more careful wit Utils files or classes because they turn to dump

case class Range(left: Int, right: Int) {
  assert(left <= right)
  def intersect(range: Range): Intersect =
    if (left <= range.left && range.right <= right) Covers
    else if (range.left <= left && right <= range.right) Inside
    else if (left >= range.right || right <= range.left) None
    else Partial
}

sealed trait Intersect
case object Inside extends Intersect
case object None extends Intersect
case object Partial extends Intersect
case object Covers extends Intersect

object AsInt {
  def unapply(s: String): Option[Int] = try{ Some(s.toInt) } catch {
    case e: NumberFormatException => scala.None
  }
}
