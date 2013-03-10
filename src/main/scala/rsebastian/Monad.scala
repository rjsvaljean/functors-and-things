package rsebastian

trait Monad[F[_]] extends ApplicativeFunctor[F] {
  def flatMap[A, B](f: A => F[B]): F[A] => F[B]
  override def fmap[A, B](f: A => B): F[A] => F[B] = flatMap(f andThen pure[B])
  override def apply[A, B](f: F[A => B]): F[A] => F[B] = { (fa: F[A]) =>
    flatMap[A => B, B](fAtoB => fmap(fAtoB)(fa))(f)
  }
}

case class State[S, +A](transition: S => (S, A))

object Monad {
  implicit def StateMonad[S] = new Monad[({type λ[α] = State[S, α]})#λ] {
    def flatMap[A, B](f: (A) => State[S, B]): (State[S, A]) => State[S, B] = { (s: State[S, A]) =>
      State[S, B]({ s1 =>
        val (newState, a) = s.transition(s1)
        f(a).transition(newState)
      })

    }

    def pure[A]: (A) => State[S, A] = { (a: A) => State({(s: S) => (s, a)}) }
  }

}