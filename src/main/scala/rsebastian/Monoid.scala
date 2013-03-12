package rsebastian

import rsebastian.InvariantFunctors.AAtoAFunctionsFunctor

/*
 * Laws:
 * i. append must be associative
 * append(append(x, y), z) == append(x, append(y, z))
 *
 * ii. identity satisfies identity
 * append(identity, x) == append(x, identity)
 */
trait Semigroup[T] {
  def append(m1: T, m2: T): T

  def smap[A](f: T => A, g: A => T): Semigroup[A] = mkSemigroup(AAtoAFunctionsFunctor.xmap((f, g))((append _).curried))

  private def mkSemigroup[A](curriedAppend: A => A => A) = new Semigroup[A] {
    def append(m1: A, m2: A): A = curriedAppend(m1)(m2)
  }
}

object Semigroup {
  implicit def SemiGroup[T] = new Semigroup[List[T]] {
    def append(m1: List[T], m2: List[T]) = m1 ::: m2
  }

  implicit def SemiGroupInt = new Semigroup[Int] {
    def append(m1: Int, m2: Int) = m1 + m2
  }
}

trait Monoid[T] extends Semigroup[T] {
  val identity: T
}

object Monoid {
  implicit def ListMonoid[T] = new Monoid[List[T]] {
    def append(m1: List[T], m2: List[T]) = m1 ::: m2
    val identity: List[T] = Nil
  }

  implicit object IntAdditiveMonoid extends Monoid[Int] {
    def append(m1: Int, m2: Int) = m1 + m2
    val identity: Int = 0
  }
}

trait Group[T] extends Monoid[T] {
  def inverse(t: T): T
}