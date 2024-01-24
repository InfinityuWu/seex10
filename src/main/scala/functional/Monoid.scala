package functional

import functional.data.Graph

// Monoid-Trait
trait Monoid[A]:
  def empty: A
  def combine(x: A, y: A): A

// Int-Summe Monoid
object IntSum extends Monoid[Int]:
  def empty: 0
  def combine(list:List[Int]): Int =
    list match
      case Nil => empty
      case first :: rest => first + IntSum.combine(rest)

// String-Konkatenation Monoid
object StringConcat extends Monoid[String]:
  def empty: ""
  def combine(list:List[String]): String =
    list match
      case Nil => empty
      case first :: rest => first + StringConcat.combine(rest)

// Tupel-Merge Monoid
case class TupleMerge [A, B](a: Monoid[A], b: Monoid[B])
  def empty: (a.empty, b.empty)
  def combine(list: List[String]): String =
    list match
      case Nil => empty
      case first :: rest => (a.combine(first))



// Liste-Merge Monoid
case class ListMerge[A](merge: Monoid[A]) extends Monoid[List[A]]:
  def empty: List[A] = List.empty
  def combine(x: List[A], y: List[A]): List[A] = x ++ y

// Graph-Merge Monoid
case class GraphMerge[A](merge: Monoid[A]) extends Monoid[Graph[A]]:
  def empty: Graph[A] = Graph.empty
  def combine(x: Graph[A], y: Graph[A]): Graph[A] = x.merge(y)

  /*
object IntSum // TODO

object StringConcat // TODO

case class TupleMerge[A, B](a: Monoid[A], b: Monoid[B]) // TODO

case class ListMerge[A](merge: Monoid[A]) // TODO

case class GraphMerge[A](merge: Monoid[A]) // TODO

  */
