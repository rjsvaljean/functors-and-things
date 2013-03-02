package rsebastian

import org.specs2.{ScalaCheck, Specification}
import rsebastian.WhyFunctor.fizzBuzz

class FunctorSpec extends Specification with ScalaCheck { def is =
  "FizzBuzz works as expected" ! {
    import rsebastian.Functor.ListFunctor
    fizzBuzz((1 to 10).toList) must_== List("1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz")
  }

//  ! prop { (a: Int) =>
//    import rsebastian.Functor._
//    fizzBuzz((((a * 10) + 1) to ((a + 1) * 10)).toList).count(_ == "Buzz") == 2
//  }

}
