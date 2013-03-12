package rsebastian

import org.specs2.Specification
import rsebastian.InvariantFunctors.AtoAFunctionsFunctor

class InvariantFunctorSpec extends Specification { def is =
  "can map a function of type A => A" ! {
    case class Person(age: Int)
    val inc: (Int) => Int = i => i + 1
    val growOlder = AtoAFunctionsFunctor.xmap[Int, Person](Person.apply _, _.age)(inc)
    growOlder(Person(22)) must_== Person(23)
  }
}
