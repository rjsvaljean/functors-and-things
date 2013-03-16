package rsebastian

import org.specs2.Specification
import Monad._

class MonadSpec extends Specification { def is =
  "Monad laws" ^
    "Left identity" ! {
      val toListOfNandSuccOfN: Int => List[Int] = {i => List(i, i + 1)}
      ListMonad.flatMap(toListOfNandSuccOfN)(ListMonad.pure(1)) must_== toListOfNandSuccOfN(1)
    } ^
    "Right identity" ! {
      object WrongListMonad extends Monad[List] {
        def flatMap[A, B](f: (A) => List[B]): (List[A]) => List[B] = ListMonad.flatMap(f)
        def pure[A]: (A) => List[A] = {(a: A) => List(a, a)}
      }

      (OptionMonad.flatMap({i: Int => OptionMonad.pure(i)})(OptionMonad.pure(1)) must_== OptionMonad.pure(1)) and
      (WrongListMonad.flatMap({i: Int => WrongListMonad.pure(i)})(WrongListMonad.pure(1)) must_!= WrongListMonad.pure(1))
    } ^
    "Associativity" ! {
      val StringState = StateMonad[String]
      import StringState._

      val s: State[String, Double] = pure(0.0)
      val f: Double => State[String, Int] = {d => pure(d).flatMap(d => State(s => (s + d.toString, d.toInt)))}
      val g: Int => State[String, BigDecimal] = {i => pure(i).flatMap(i => State(s => (s + i.toString, BigDecimal(i) * 1000000000)))}

      flatMap(g)(flatMap(f)(s)).transition("hello") must_== flatMap((d: Double) => flatMap(g)(f(d)))(s).transition("hello")
    } ^ bt ^
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
