package rsebastian

import org.specs2.Specification
import rsebastian.OptionHelpers._
import rsebastian.WhyApplicativeFunctor.{sequence, validate}
import org.specs2.matcher.DataTables

class ApplicativeFunctorSpec extends Specification with DataTables { def is =
  "sequence should work as expected" ^
    "for a list of Options" ! {
      (sequence(List[Option[Nothing]]()) must_== just(Nil)) and
      (sequence(List(just(1), just(2), just(3))) must_== just(List(1, 2, 3))) and
      (sequence(List(just(1), void[Int], just(3))) must_== void[Int])
    } ^
    "for a list of lists" ! {
      (sequence(List(List(1, 2), List(5, 6))) must_== List(List(1, 5), List(1, 6), List(2, 5), List(2, 6)))
    } ^ bt ^
  "validation should work as expected" ! {
    "number" | "string" |"expected failure"                                                  |
     50      ! "abC"    !List("String is in the wrong format")                               |
    -50      ! "ABC"    !List("Number is not within range")                                  |
    -50      ! "blah"   !List("Number is not within range", "String is in the wrong format") |> { (s, n, r) =>
      (validate(s, n) match {
        case Failure(error) => error
        case Success(_)     => Nil
      }) must_== r
    }
  } ^
  "can traverse and collect contents of binary tree" ! {
    val tree: BinaryTree[String] = Bin(Leaf("hello"), Bin(Leaf("world"), Leaf("and stuff")))
    Traversable.Traversers.contents(tree) must_== List("hello", "world", "and stuff")
  } ^
  "can count the number of elements in the tree" ! {
    val tree: BinaryTree[String] = Bin(Leaf("hello"), Bin(Leaf("world"), Leaf("and stuff")))
    Traversable.Traversers.count(tree) must_== 3
  }
}
