package rsebastian

trait Functor[F[_]] {
  def fmap[A, B](f: A => B): F[A] => F[B]
}

object Functor {
  implicit object ListFunctor extends Functor[List] {
    def fmap[A, B](f: A => B) = _.foldLeft(List[B]())(_ :+ f(_))
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