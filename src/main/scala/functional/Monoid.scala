package functional

import functional.data.Graph

// Monoid-Trait
trait Monoid[A]:
  def empty: A
  def combine(x: A, y: A): A

// Int-Summe Monoid
object IntSum extends Monoid[Int]:
  def add(list:List[Int]): Int =
    list match
      case Nil => 0
      case first :: rest => first + IntSum.add(rest)

// String-Konkatenation Monoid
object StringConcat extends Monoid[String]:
  def concat(list:List[String]): String =
    list match
      case Nil => ""
      case first :: rest => first + StringConcat.concat(rest)

// Tupel-Merge Monoid
case class TupleMerge extends Monoid[(Int, String)]:

  def merge(list: List[(Int, String)]): (Int, String) =
    (IntSum.add(list.map((a,b) => a)), StringConcat.concat(list.map((a,b) => b)))


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
