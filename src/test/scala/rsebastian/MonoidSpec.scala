package rsebastian

import org.specs2.Specification
import rsebastian.Monoid.IntAdditiveMonoid

/*
 * Laws:
 * i. append must be associative
 * append(append(x, y), z) == append(x, append(y, z))
 *
 * ii. identity satisfies identity
 * append(identity, x) == append(x, identity)
 */

class MonoidSpec extends Specification { def is =
  "Monoid Laws" ^
    "Associativity" ! {
      import IntAdditiveMonoid.{append => intAppend}

      object IntModuloMonoid extends Monoid[Int] {
        def append(m1: Int, m2: Int) = m1 % m2
        val identity: Int = 0
      }

      val nonAssociativeAppend = IntModuloMonoid.append _

      (intAppend(intAppend(1, 2), 3) must_== intAppend(1, intAppend(2, 3))) and
      (nonAssociativeAppend(nonAssociativeAppend(10, 7), 5) must_!= nonAssociativeAppend(10, nonAssociativeAppend(7, 5)))
    } ^
    "Identity" ! {
      import IntAdditiveMonoid.{append => intAppend, identity => intIdentity}

      (intAppend(intIdentity, 3) must_== intAppend(3, intIdentity))
    }
}
