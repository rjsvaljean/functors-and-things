package rsebastian

import ContravariantFunctorHelpers._

trait  ContravariantFunctor[F[_]] {
  def contramap[A, B](f: B => A): F[A] => F[B]
}

object ContravariantFunctor {

  object OrderingFunctor extends ContravariantFunctor[Ordering] {
    def contramap[A, B](f: (B) => A): (Ordering[A]) => Ordering[B] = { (oa: Ordering[A]) =>
      new Ordering[B] {
        def compare(xB: B, yB: B): Boolean = oa.compare(f(xB), f(yB))
      }
    }
  }

  trait FunctionWithFixedOutFunctor[T] extends ContravariantFunctor[({type λ[α] = Function1[α, T]})#λ] {
    def contramap[A, B](f: B => A): (A => T) => B => T = { (fa: A => T) =>
      (b : B) => fa(f(b))
    }
  }
}

object WhyContravarianFunctors {
  case class Pun(pun: String, awesomeness: Int)
  val orderPunByAwesomeness = ContravariantFunctor.OrderingFunctor.contramap[Int, Pun](_.awesomeness)(intOrder)
  def punToTheDeath(pun1: Pun, pun2: Pun) = if (orderPunByAwesomeness.compare(pun1, pun2)) pun1 else pun2
}

object ContravariantFunctorHelpers {
  trait Ordering[T] {
    def compare(x: T, y: T): Boolean
  }

  val intOrder = new Ordering[Int] {
    def compare(x: Int, y: Int) = x > y
  }
}