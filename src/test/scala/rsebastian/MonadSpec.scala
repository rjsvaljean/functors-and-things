package rsebastian

import org.specs2.Specification

class MonadSpec extends Specification { def is =
  "State monad" ^
    "accumulates state changing computations" ! {
      def add(i: Int) = State((j: Int) => (j + i, ()))
      val changeStateBy1 = add(1)
      val changeStateBy2 = Monad.StateMonad[Int].flatMap[Unit, Unit](_ => add(2))(changeStateBy1)
      changeStateBy2.transition(0)._1 must_== 3
    }
}
