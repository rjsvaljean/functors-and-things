package rsebastian


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
}

object Semigroup {
  implicit def SemiGroup[T] = new Semigroup[List[T]] {
    def append(m1: List[T], m2: List[T]) = m1 ::: m2
  }
}

trait Monoid[T] extends Semigroup[T] {
  val identity: T
}

trait Group[T] extends Monoid[T] {
  def inverse(t: T): T
}