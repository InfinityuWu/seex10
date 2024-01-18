package functional

import functional.data.Graph

trait Monoid[A]:
  def empty: A
  def combine(x: A, y: A): A

object IntSum // TODO

object StringConcat // TODO

case class TupleMerge[A, B](a: Monoid[A], b: Monoid[B]) // TODO

case class ListMerge[A](merge: Monoid[A]) // TODO

case class GraphMerge[A](merge: Monoid[A]) // TODO
