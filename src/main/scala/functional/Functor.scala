package functional

import functional.data.{Graph, Tree}

trait Functor[F[_]]:
  def map[A, B](f: A => B): F[A] => F[B]

object ListFunctor extends Functor[List]:
  override def map[A, B](f: A => B): List[A] => List[B] =
    case Nil      => Nil
    case hd :: tl => f(hd) :: map(f)(tl)

object OptionFunctor extends Functor[Option]:
  override def map[A, B](f: A => B): Option[A] => Option[B] =
    case None    => None
    case Some(a) => Some(f(a))

object TreeFunctor extends Functor[Tree]:
  override def map[A, B](f: A => B): Tree[A] => Tree[B] =
    case Tree.Empty => Tree.Empty
    case Tree.Node(l, a, r) =>
      Tree.Node(map(f)(l), f(a), map(f)(r))

object GraphFunctor extends Functor[Graph]:
  override def map[A, B](f: A => B): Graph[A] => Graph[B] =
    case Graph(Nil, Nil) => Graph(f(Nil.asInstanceOf[A]), Nil)
    case Graph(element, neighbors) => Graph(f(element), neighbors.map(map(f)))
      

def capitalizeNames(nameList: List[Option[String]]): List[Option[String]] =
  ListFunctor.map((opt: Option[String]) => OptionFunctor.map((s: String) => s.capitalize)(opt))(nameList)