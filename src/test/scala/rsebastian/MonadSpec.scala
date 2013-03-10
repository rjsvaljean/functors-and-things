package rsebastian

import org.specs2.Specification
import Monad._

class MonadSpec extends Specification { def is =
  "State monad" ^
    "accumulates state changing computations" ! {

      val count = (s: String) => State((n: Int) => (n+1, s + n))

      ((count("a-") flatMap count flatMap count).transition(0) must_== (3, "a-012")) and
      ((for {
        st1 <- count("a-")
        st2 <- count(st1)
        st3 <- count(st2)
      } yield st3).transition(0) must_== (3, "a-012"))
    }
}
