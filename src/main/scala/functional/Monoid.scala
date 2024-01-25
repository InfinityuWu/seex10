package functional

import functional.data.Graph

// Monoid-Trait
trait Monoid[A]:
  def empty: A
  def combine(x: A, y: A): A

// Int-Summe Monoid
object IntSum extends Monoid[Int]:
  def empty = 0
  def combine(x: Int, y: Int): Int = x + y

object StringConcat extends Monoid[String]:
  def empty = ""
  def combine(x: String, y: String): String = x + y

case class TupleMerge[A, B](a: Monoid[A], b: Monoid[B]) extends Monoid[(A, B)] {
  def empty: (A, B) = (a.empty, b.empty)
  def combine(x: (A, B), y: (A, B)): (A, B) = (a.combine(x._1, y._1), b.combine(x._2, y._2))
}

case class ListMerge[A](merge: Monoid[A]) extends Monoid[List[A]]:
  def empty: List[A] = List.empty
  def combine(x: List[A], y: List[A]): List[A] =
    x.zipAll(y, merge.empty, merge.empty).collect { case (a, b) => merge.combine(a, b) }

case class GraphMerge[A](merge: Monoid[A]) extends Monoid[Graph[A]] {
  def empty: Graph[A] = Graph(merge.empty, Nil)

  def combine(x: Graph[A], y: Graph[A]): Graph[A] =
    Graph(merge.combine(x.element, y.element), ListMerge[Graph[A]](GraphMerge(merge)).combine(x.neighbors, y.neighbors))
}

