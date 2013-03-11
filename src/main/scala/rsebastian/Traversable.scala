package rsebastian

import Monad.State

trait Traversable[T[_]] {
  def traverse[F[_], A, B](f: A => F[B])(implicit app: ApplicativeFunctor[F]): T[A] => F[T[B]]

  def contents[A]: (T[A]) => List[A] = reduce({(i: A) => List(i)})

  def count[A]: T[A] => Int = reduce((a: A) => 1)

  def reduce[A, M](reducer: A => M)(implicit m: Monoid[M]): (T[A]) => M = { (ta: T[A]) =>
    implicit def a = ApplicativeFunctor.MonoidApplicativeFunctor[M]
    val traverser = traverse[({type λ[+α] = Ident[M]})#λ, A, A]({i: A => Ident(reducer(i))})
    traverser(ta).value
  }

  def map[A, B](f: A => B): T[A] => T[B] = {(ta: T[A]) =>
    implicit def a = ApplicativeFunctor.IdentApplicativeFunctor
    val traverser = traverse[Ident, A, B]({i: A => Ident(f(i))})
    traverser(ta).value
  }

  def decompose[A]: T[A] => (T[Unit], List[A]) = { (ta: T[A]) =>
    val shape: (A) => Ident[Unit] = (a: A) => Ident(())
    val content: (A) => Ident[List[A]] = (a: A) => Ident(List(a))
    val product: (A) => Product[Ident, ({type λ[α] = Ident[List[A]]})#λ, Unit] = { (a: A) =>
      Product[Ident, ({type Φ[β] = Ident[List[A]]})#Φ, Unit](shape(a), content(a))
    }

    implicit def app = ApplicativeFunctor.ProductApplicativeFunctor[Ident, ({type Φ[β] = Ident[List[A]]})#Φ]

    val traverser = traverse[({type λ[α] = Product[Ident, ({type Φ[β] = Ident[List[A]]})#Φ, α]})#λ, A, Unit](product)
    val (Ident(s), Ident(c)) = traverser(ta).tuple
    (s, c)
  }

  def collect[F[_] : ApplicativeFunctor, A, B](f: A => F[Unit], g: A => B): T[A] => F[T[B]] = {
    val app = implicitly[ApplicativeFunctor[F]]
    val application = (a: A) => app.apply(app.pure((u: Unit) => g(a)))(f(a))
    traverse[F, A, B](application)
  }

  def disperse[F[_] : ApplicativeFunctor, A, B, C](f: F[B], g: A => B => C): T[A] => F[T[C]] = {
    val app = implicitly[ApplicativeFunctor[F]]
    traverse[F, A, C]({(a:A) => app.apply(app.pure(g(a)))(f)})
  }

  def measure[F[_] : ApplicativeFunctor, A, B](f: F[Unit], g: A => B): T[A] => F[T[B]] = {
    val app = implicitly[ApplicativeFunctor[F]]
    traverse[F, A, B]({(a: A) => app.apply(app.pure({(u: Unit) => g(a)}))(f)})
  }

  def assemble[A](shape: T[Unit], contents: List[A]): (List[A], Option[T[A]]) = {

    implicit def y: ApplicativeFunctor[({type λ[α] = State[List[A], Option[α]]})#λ] = ApplicativeFunctor.of[({type λ[α] = State[List[A], α]})#λ].compose(ApplicativeFunctor.of[Option])

    val x = traverse[({type λ[α] = State[List[A], Option[α]]})#λ, Unit, A]((u: Unit) => State[List[A], Option[A]]({s => s.headOption match {
      case Some(h) => (s.tail, Some(h))
      case None    => (Nil, None)
    }}))
    x(shape).transition(contents)
  }
}

sealed trait BinaryTree[A]
case class Leaf[A](a: A) extends BinaryTree[A]
case class Bin[A](left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

object Traversable {
  def of[T[_] : Traversable] = implicitly[Traversable[T]]
  object Traversers {
    def contents[T[_], A](ta: T[A])(implicit tr: Traversable[T]) = tr.contents(ta)
    def count[T[_], A](ta: T[A])(implicit tr: Traversable[T]) = tr.count(ta)
    def map[T[_], A, B](f: A => B)(ta: T[A])(implicit tr: Traversable[T]) = tr.map(f)(ta)
    def shape[T[_], A, B](ta: T[A])(implicit tr: Traversable[T]) = tr.map((_: A) => ())(ta)
    def decompose[T[_], A](ta: T[A])(implicit tr: Traversable[T]): (T[Unit], List[A]) = tr.decompose(ta)
  }

  implicit object BinaryTreeTraversable extends Traversable[BinaryTree] {
    def traverse[F[+_], A, B](f: (A) => F[B])(implicit app: ApplicativeFunctor[F]): (BinaryTree[A]) => F[BinaryTree[B]] = {
      case Bin(l, r) => app.apply(app.apply(app.pure((Bin.apply[B] _).curried))(traverse(f)(app)(l)))(traverse(f)(app)(r)) : F[BinaryTree[B]]
      case Leaf(a)   => app.apply(app.pure(Leaf.apply[B] _))(f(a))
    }
  }
}