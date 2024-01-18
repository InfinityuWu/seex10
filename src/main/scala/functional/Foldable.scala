package functional

import functional.data.{Graph, Tree}

trait Foldable[F[_]]:
  def fold[A](t: F[A])(m: Monoid[A]): A
  def foldMap[A, B](t: F[A])(f: A => B)(m: Monoid[B]): B

object ListFoldable extends Foldable[List]:
  override def fold[A](t: List[A])(m: Monoid[A]): A = t match
    case Nil      => m.empty
    case hd :: tl => m.combine(hd, fold(tl)(m))

  override def foldMap[A, B](t: List[A])(f: A => B)(m: Monoid[B]): B = ???

object TreeFoldable // TODO

object GraphFoldable // TODO

def wordCount[F[_]](t: F[String])(f: Foldable[F]): Int = ???

def charCount[F[_]](t: F[String])(f: Foldable[F]): Int = ???
