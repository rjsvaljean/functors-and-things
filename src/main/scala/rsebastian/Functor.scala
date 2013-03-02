package rsebastian

trait Functor[F[_]] {
  def fmap[A, B](f: A => B): F[A] => F[B]
}

object Functor {
  implicit object ListFunctor extends Functor[List] {
    def fmap[A, B](f: A => B) = _.foldLeft(List[B]())(_ :+ f(_))
//    def fmap[A, B](f: A => B) = _.map(f)
  }

  implicit object OptionFunctor extends Functor[Option] {
    def fmap[A, B](f: A => B) = _.map(f)
  }
}

object WhyFunctor {
  def fizzBuzz[F[_]](xs: F[Int])(implicit f: Functor[F]) = f.fmap[Int, String]({ i =>
    if      (i     == 0) "0"
    else if (i % 3 == 0) "Fizz"
    else if (i % 5 == 0) "Buzz"
    else                 i.toString
  })(xs)
}

object OptionHelpers {
  def just[A](a: A): Option[A] = Some(a)
  def void: Option[Nothing] = None
}