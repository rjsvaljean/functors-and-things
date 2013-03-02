package rsebastian

import org.specs2.{ScalaCheck, Specification}
import rsebastian.WhyFunctor.fizzBuzz
import rsebastian.OptionHelpers._

class FunctorSpec extends Specification with ScalaCheck { def is =
  "FizzBuzz works as expected" ^
    "for Lists" ! {
      import rsebastian.Functor.ListFunctor
      fizzBuzz((1 to 10).toList) must_== List("1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz")
    } ^
    "for Options" ! {
      import rsebastian.Functor.OptionFunctor
      (fizzBuzz(just(1)) must_== just("1")) and
      (fizzBuzz(just(9)) must_== just("Fizz")) and
      (fizzBuzz(just(5)) must_== just("Buzz")) and
      (fizzBuzz(void) must_== void)
    }
  "The List functor obeys the functor laws" ^
    "if we map the id function over a functor, the functor that we get back should be the same as the original functor" ! {
      (functorOf[List].fmap[Nothing, Nothing](identity[Nothing])(List()) must_== List()) and
        (functorOf[List].fmap(identity[Int])(List(1, 2, 3)) must_== List(1, 2, 3))
    } ^
    """composing two functions and then mapping the resulting function over a functor
      |should be the same as first mapping one function over the functor and then mapping the other one.""".stripMargin ! {
      val toStr = {(i: Int) => i.toString}
      val appendThing = {(i: String) => i + " thing"}
      (functorOf[List].fmap(toStr andThen appendThing)(List(1, 2)) must_== functorOf[List].fmap(appendThing)(functorOf[List].fmap(toStr)(List(1, 2))))
    }
  end

  def functorOf[F[_]](implicit f: Functor[F]) = f
}
