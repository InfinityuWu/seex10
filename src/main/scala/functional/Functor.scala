package functional

import functional.data.{Graph, Tree}

trait Functor[F[_]]:
  def map[A, B](f: A => B): F[A] => F[B]

object ListFunctor extends Functor[List]:
  override def map[A, B](f: A => B): List[A] => List[B] =
    case Nil      => Nil
    case hd :: tl => f(hd) :: map(f)(tl)

object OptionFunctor // TODO

object TreeFunctor // TODO

object GraphFunctor // TODO

def capitalizeNames(nameList: List[Option[String]]) = ???
