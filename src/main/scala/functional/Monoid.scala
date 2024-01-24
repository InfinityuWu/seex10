package functional

import functional.data.Graph

trait Monoid[A]:
  def empty: A
  def combine(x: A, y: A): A

// Monoid-Trait
trait Monoid[A]:
  def empty: A
  def combine(x: A, y: A): A

// Int-Summe Monoid
object IntSum extends Monoid[Int]:
  def empty: Int = 0
  def combine(x: Int, y: Int): Int = x + y

// String-Konkatenation Monoid
object StringConcat extends Monoid[String]:
  def empty: String = ""
  def combine(x: String, y: String): String = x + y

// Tupel-Merge Monoid
case class TupleMerge[A, B](a: Monoid[A], b: Monoid[B]) extends Monoid[(A, B)]:
  def empty: (A, B) = (a.empty, b.empty)
  def combine(x: (A, B), y: (A, B)): (A, B) = (a.combine(x._1, y._1), b.combine(x._2, y._2))

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
