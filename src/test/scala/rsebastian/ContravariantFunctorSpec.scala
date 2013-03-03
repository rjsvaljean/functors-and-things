package rsebastian

import org.specs2.Specification
import rsebastian.WhyContravarianFunctors._
import rsebastian.ContravariantFunctor.OrderingFunctor
import rsebastian.ContravariantFunctorHelpers.intOrder

class ContravariantFunctorSpec extends Specification { def is =
  "Pun showdown works as expected" ! {
    val good = Pun("Freudian slip: worst fad in ladies fashion ever", 1)
    val better = Pun("Freudian slip: when you mean one thing but say mother", 2)
    (punToTheDeath(good, better) must_== better) and
    (punToTheDeath(better, good) must_== better)
  } ^
  "Ordering fuctor must obey the contravariant functor laws" ^
    """if we map the id function over a contravariant functor,
      |the functor that we get back should be the same as the original functor
      |
      | contramap id = id
      |
      |""".stripMargin ! {
      OrderingFunctor.contramap(identity[Int])(intOrder).compare(1, 2) must_== intOrder.compare(1, 2)
    } ^
    """composing two functions and then mapping the resulting function over a functor
      |should be the same as first mapping the second function over the functor and then mapping the other one.
      |
      | contramap f . contramap g = contramap (g . f)
      |
      |""".stripMargin ! {
      val toLength = { (s: String) => s.length }
      val modulo5 = { (i: Int) => i % 5 }
      (OrderingFunctor.contramap(toLength andThen modulo5)(intOrder).compare("hello", "hey")).must_==(
        OrderingFunctor.contramap(toLength)(OrderingFunctor.contramap(modulo5)(intOrder)).compare("hello", "hey")
      )
    }
}
