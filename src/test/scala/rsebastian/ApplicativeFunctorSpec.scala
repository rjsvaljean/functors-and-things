package rsebastian

import org.specs2.Specification
import rsebastian.OptionHelpers._
import rsebastian.WhyApplicativeFunctor.{sequence, validate}
import org.specs2.matcher.DataTables
import rsebastian.Monad.State

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
  } ^
  "can map over the element of the tree" ! {
    val tree: BinaryTree[String] = Bin(Leaf("hello"), Bin(Leaf("world"), Leaf("and stuff")))
    Traversable.Traversers.map((_: String).length)(tree) must_== Bin(Leaf(5), Bin(Leaf(5), Leaf(9)))
  } ^
  "can map and get contents simultaneously" ! {
    val tree: BinaryTree[String] = Bin(Leaf("hello"), Bin(Leaf("world"), Leaf("and stuff")))
    Traversable.Traversers.decompose(tree) must_== (Bin(Leaf(()), Bin(Leaf(()), Leaf(()))), List("hello", "world", "and stuff"))
  } ^
  "can collect state and map simultaneously" ! {
    val tree: BinaryTree[String] = Bin(Leaf("hello"), Bin(Leaf("world"), Bin(Leaf("erm..."), Leaf("more stuff?"))))
    def count(s: String) = State((i: Int) => (i + 1, ()))
    def toLength(s: String) = s.length
    val x = Traversable.of(tree).collect[({type λ[+α] = State[Int, α]})#λ, String, Int](count, toLength)
    x(tree).transition(0) must_== (4, Bin(Leaf(5), Bin(Leaf(5), Bin(Leaf(6), Leaf(11)))))
  } ^
  "disperse works as expected" ! {
    val tree: BinaryTree[Double] = Bin(Leaf(1.1), Bin(Leaf(2.2), Leaf(3.3)))
    val labelling: State[Int, Int] = State((n: Int) => (n+1, n+1))
    val naming: Double => Int => String = (p1: Double) => (p2: Int) => p1+" is at "+p2

    val x = Traversable.of(tree).disperse[({type λ[+α] = State[Int, α]})#λ, Double, Int, String](labelling, naming)
    x(tree).transition(0)._2 must_== Bin(Leaf("1.1 is at 1"), Bin(Leaf("2.2 is at 2"), Leaf("3.3 is at 3")))
  }

}
