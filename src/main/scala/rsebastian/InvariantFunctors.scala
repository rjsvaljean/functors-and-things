package rsebastian

/**
 * http://comonad.com/reader/2008/rotten-bananas/
 * http://hackage.haskell.org/packages/archive/category-extras/0.53.5/doc/html/Control-Functor-Exponential.html
 * Not sure why they're useful. Keeping them in here for completeness.
 *
 * Unresolved Qs:
 * * If the base type of the invariant functor is invariant what is the point of leaving it polymorphic?
 *   i.e. The ConstantTypeFunctor seems to be of little value
 * * What are the laws governing invariant functors?
 * * Is a Monoid an invariant functor?
 */
trait InvariantFunctors[F[_]] { // Also called exponential functors
  def xmap[A, B](f: (A => B, B => A)): F[A] => F[B]
}

object InvariantFunctors {
  trait ConstantTypeFunctor[T] extends InvariantFunctors[({type λ[α] = T})#λ] {
    def xmap[A, B](f: (A => B, B => A)) = identity[T]
  }

  object AtoAFunctionsFunctor extends InvariantFunctors[({type λ[α] = α => α})#λ] {
    def xmap[A, B](f: ((A) => B, (B) => A)): ((A) => A) => (B) => B = { (a2a: A => A) =>
      f._2 andThen a2a andThen f._1
    }
  }

  trait FunctorsAreExponentialFunctors[F[_]] extends Functor[F] with InvariantFunctors[F] {
    def fmap[A, B](f: A => B): F[A] => F[B]
    def xmap[A, B](f: (A => B, B => A)): F[A] => F[B] = fmap(f._1)
  }

  trait ContravariantFunctorsAreExponentialFunctors[F[_]] extends ContravariantFunctor[F] with InvariantFunctors[F] {
    def contramap[A, B](f: B => A): F[A] => F[B]
    def xmap[A, B](f: (A => B, B => A)): F[A] => F[B] = contramap(f._2)
  }

}
