package rsebastian

trait Monad[F[_]] extends ApplicativeFunctor[F] {
  def flatMap[A, B](f: A => F[B]): F[A] => F[B]
  override def fmap[A, B](f: A => B): F[A] => F[B] = flatMap(f andThen pure[B])
  override def apply[A, B](f: F[A => B]): F[A] => F[B] = { (fa: F[A]) =>
    flatMap[A => B, B](fAtoB => fmap(fAtoB)(fa))(f)
  }
}

object Monad {
}