package rsebastian

import rsebastian.OptionHelpers._
import rsebastian.Monad.State

trait ApplicativeFunctor[F[_]] extends Functor[F]{
  def pure[A]: A => F[A]
  def apply[A, B](f: F[A => B]): F[A] => F[B]
  def fmap[A, B](f: A => B): F[A] => F[B] = (pure[A => B] andThen apply)(f)
  def compose[G[_]](gApp: ApplicativeFunctor[G]) = composedApp(this, gApp)

  def liftA2[A, B, C](f: A => B => C): F[A] => F[B] => F[C] = {(fa: F[A]) => apply(fmap(f)(fa))}

  private def composedApp[F1[_], G[_]](fApp: ApplicativeFunctor[F1], gApp: ApplicativeFunctor[G]) = new ApplicativeFunctor[({type λ[α] = F1[G[α]]})#λ] {
    def pure[A]: (A) => F1[G[A]] = { (a: A) => fApp.pure(gApp.pure(a)) }
    def apply[A, B](f: F1[G[(A) => B]]): (F1[G[A]]) => F1[G[B]] = fApp.liftA2({(gAtoB: G[A => B]) => gApp.apply(gAtoB)})(f)
  }
}

case class Ident[A](value: A)

case class Product[F1[_], F2[_], A](first: F1[A], second: F2[A]) {
  def tuple = (first, second)
}

sealed trait Validation[+E, +X]
case class Failure[E, X](e: E) extends Validation[E, X]
case class Success[E, X](a: X) extends Validation[E, X]

object ApplicativeFunctor {
  def of[F[_] : ApplicativeFunctor] = implicitly[ApplicativeFunctor[F]]


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
