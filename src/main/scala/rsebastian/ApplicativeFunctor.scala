package rsebastian

import rsebastian.OptionHelpers._

trait ApplicativeFunctor[F[_]] extends Functor[F]{
  def pure[A]: A => F[A]
  def apply[A, B](f: F[A => B]): F[A] => F[B]
  def fmap[A, B](f: A => B): F[A] => F[B] = (pure[A => B] andThen apply)(f)
}

trait Traversable[T[_]] {
  def traverse[F[_], A, B](f: A => F[B])(implicit app: ApplicativeFunctor[F]): T[A] => F[T[B]]
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

  sealed trait BinaryTree[A]
  case class Leaf[A](a: A) extends BinaryTree[A]
  case class Bin[A](left: BinaryTree[A], right: BinaryTree[A]) extends BinaryTree[A]

  object BinaryTreeTraversable extends Traversable[BinaryTree] {
    def traverse[F[+_], A, B](f: (A) => F[B])(implicit app: ApplicativeFunctor[F]): (BinaryTree[A]) => F[BinaryTree[B]] = {
      case Bin(l, r) => app.apply(app.apply(app.pure((Bin.apply[B] _).curried))(traverse(f)(app)(l)))(traverse(f)(app)(r)) : F[BinaryTree[B]]
      case Leaf(a)   => app.apply(app.pure(Leaf.apply[B] _))(f(a))
    }
  }

  case class Const[M, +A](value: M)

  implicit def ConstApplicativeFunctor[M : Monoid] = new ApplicativeFunctor[({type λ[+α] = Const[M, α]})#λ] {
    def pure[A]: (A) => Const[M, A] = (a: A) => Const[M, A](implicitly[Monoid[M]].identity)

    def apply[A, B](f: Const[M, A => B]): (Const[M, A]) => Const[M, B] = { (ca: Const[M, A]) =>
      Const[M, B](implicitly[Monoid[M]].append(ca.value, f.value))
    }
  }

  def getSizeOfItemsInTree(tree: BinaryTree[String]): List[Int] = {
    val x = BinaryTreeTraversable.traverse[({type λ[+α] = Const[List[Int], α]})#λ, String, Int]({i: String => Const(List(i.size))})
    x(tree).value
  }
}
