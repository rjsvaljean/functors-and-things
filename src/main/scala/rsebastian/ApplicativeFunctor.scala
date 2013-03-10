package rsebastian

import rsebastian.OptionHelpers._

trait ApplicativeFunctor[F[_]] extends Functor[F]{
  def pure[A]: A => F[A]
  def apply[A, B](f: F[A => B]): F[A] => F[B]
  def fmap[A, B](f: A => B): F[A] => F[B] = (pure[A => B] andThen apply)(f)
}

case class Ident[A](value: A)

case class Product[F1[_], F2[_], A](first: F1[A], second: F2[A]) {
  def tuple = (first, second)
}


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

}



sealed trait BinaryTree[A]
case class Leaf[A](a: A) extends BinaryTree[A]
case class Bin[A](left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]



object Traversable {
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

sealed trait Validation[+E, +X]
case class Failure[E, X](e: E) extends Validation[E, X]
case class Success[E, X](a: X) extends Validation[E, X]

object ApplicativeFunctor {
  implicit object OptionApplicativeFunctor extends ApplicativeFunctor[Option] {
    def pure[A]: (A) => Option[A] = just
    def apply[A, B](f: Option[A => B]): Option[A] => Option[B] = f match {
      case Some(fun) => _.map(fun)
      case None      => _ => void[B]
    }
  }

  implicit object ListApplicativeFunctor extends ApplicativeFunctor[List] {
    def pure[A]: (A) => List[A] = a => List(a)
    def apply[A, B](f: List[A => B]): List[A] => List[B] = xs => for {
      anF <- f
      x <- xs
    } yield anF(x)
  }

  class ValidationApplicativeFunctor[E](implicit sg: Semigroup[E]) extends ApplicativeFunctor[({type λ[α] = Validation[E, α]})#λ] {
    def pure[A]: (A) => Validation[E, A] = Success.apply
    def apply[A, B](f: Validation[E, A => B]): (Validation[E, A]) => Validation[E, B] = va => (f, va) match {
      case (Failure(e1), Failure(e2))  => Failure(sg.append(e1, e2))
      case (Failure(e1), Success(_))   => Failure(e1)
      case (Success(_), Failure(e2))   => Failure(e2)
      case (Success(foo), Success(sa)) => Success(foo(sa))
    }
  }

  implicit def MonoidApplicativeFunctor[M : Monoid] = new ApplicativeFunctor[({type λ[+α] = Ident[M]})#λ] {
    def pure[A]: (A) => Ident[M] = (a: A) => Ident(implicitly[Monoid[M]].identity)

    def apply[A, B](f: Ident[M]): (Ident[M]) => Ident[M] = { (in: Ident[M]) =>
      Ident[M](implicitly[Monoid[M]].append(f.value, in.value))
    }
  }

  implicit object IdentApplicativeFunctor extends ApplicativeFunctor[Ident] {
    def pure[A]: (A) => Ident[A] = Ident.apply[A]
    def apply[A, B](f: Ident[A => B]): (Ident[A]) => Ident[B] = {(_ : Ident[A]).value} andThen f.value andThen Ident.apply
  }

  implicit def ProductApplicativeFunctor[F1[_], F2[_]](implicit f1App: ApplicativeFunctor[F1], f2App: ApplicativeFunctor[F2]) = new ApplicativeFunctor[({type λ[α] = Product[F1, F2, α]})#λ]{
    def pure[A]: (A) => Product[F1, F2, A] = { (a: A) => Product(f1App.pure(a), f2App.pure(a)) }

    def apply[A, B](f: Product[F1, F2, (A) => B]): (Product[F1, F2, A]) => Product[F1, F2, B] = { (pa: Product[F1, F2, A]) =>
      Product(f1App.apply(f.first)(pa.first), f2App.apply(f.second)(pa.second))
    }
  }
}

object WhyApplicativeFunctor {
  import ApplicativeFunctor._
  import rsebastian.Semigroup._

  def addInContext[F[_]](x1: F[Int], x2: F[Int])(implicit app: ApplicativeFunctor[F]): F[Int] = {
    val curriedAddition: Int => Int => Int = i => j => i + j
    app.apply(app.apply(app.pure(curriedAddition))(x1))(x2)
  }

  def validate(number: Int, string: String) = {
    def validNumber(n: Int) = if (n < 100 && n > 0) Success(n) else Failure(List("Number is not within range"))
    def validString(s: String) = if (s.matches("""[A-Z]{3}""")) Success(s) else Failure(List("String is in the wrong format"))

    case class Result(number: Int, string: String)

    val app = new ValidationApplicativeFunctor[List[String]]()

    app.apply(app.apply(app.pure((Result(_, _)).curried))(validNumber(number)))(validString(string))
  }

  def sequence[F[_], T](xs: List[F[T]])(implicit app: ApplicativeFunctor[F]): F[List[T]] = {
    val curriedListAppend: List[T] => T => List[T] = is => i => is :+ i
    xs.foldLeft(app.pure(List[T]())) { (sum, x) =>
      app.apply(app.fmap(curriedListAppend)(sum))(x)
    }
  }
}
