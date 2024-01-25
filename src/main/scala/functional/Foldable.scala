package functional

import functional.data.{Graph, Tree}

trait Foldable[F[_]]:
  def fold[A](t: F[A])(m: Monoid[A]): A
  def foldMap[A, B](t: F[A])(f: A => B)(m: Monoid[B]): B

object ListFoldable extends Foldable[List]:
  override def fold[A](t: List[A])(m: Monoid[A]): A = t match
    case Nil      => m.empty
    case hd :: tl => m.combine(hd, fold(tl)(m))

  override def foldMap[A, B](t: List[A])(f: A => B)(m: Monoid[B]): B = fold[B](t.map(f))(m)

object TreeFoldable extends Foldable[Tree]:
  override def fold[A](t: Tree[A])(m: Monoid[A]): A = t match
    case Tree.Empty      => m.empty
    case Tree.Node(l, e, r) => m.combine(m.combine(fold(l)(m), fold(r)(m)), e)

  override def foldMap[A, B](t: Tree[A])(f: A => B)(m: Monoid[B]): B = t match
    case Tree.Empty      => m.empty
    case Tree.Node(l, e, r) => m.combine(m.combine(foldMap(l)(f)(m), foldMap(r)(f)(m)), f(e))

object GraphFoldable extends Foldable[Graph]:
  override def fold[A](t: Graph[A])(m: Monoid[A]): A = t match
    case Graph(element, Nil) => element
    case Graph(element, neighbors)   =>
      val graphMerge: GraphMerge[A] = new GraphMerge[A](m)
      m.combine(t.element, fold(ListFoldable.fold(t.neighbors)(graphMerge))(m))

  override def foldMap[A, B](t: Graph[A])(f: A => B)(m: Monoid[B]): B = t.element match {
    case Nil => m.empty
    case _ => m.empty//m.combine(f(t.element), ListFoldable.foldMap(t.neighbors)(f)(m))
  }


def wordCount[F[_]](t: F[String])(f: Foldable[F]): Int = ???

def charCount[F[_]](t: F[String])(f: Foldable[F]): Int = ???
