package rsebastian

import org.specs2.Specification
import rsebastian.Monad.State

class TraversableSpec extends Specification { def is =
  "From the Essence of the Iterator Pattern. Traversable:" ^
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
      def count(s: String) = State((i: Int) => (i + s.length, ()))
      def toLength(s: String) = s.length
      val x = Traversable.of[BinaryTree].collect[({type λ[+α] = State[Int, α]})#λ, String, Int](count, toLength)
      x(tree).transition(0) must_== (27, Bin(Leaf(5), Bin(Leaf(5), Bin(Leaf(6), Leaf(11)))))
    } ^
    "disperse works as expected" ! {
      val tree: BinaryTree[Double] = Bin(Leaf(1.1), Bin(Leaf(2.2), Leaf(3.3)))
      val labelling: State[Int, Int] = State((n: Int) => (n+1, n+1))
      val naming: Double => Int => String = (p1: Double) => (p2: Int) => p1+" is at "+p2

      val x = Traversable.of[BinaryTree].disperse[({type λ[+α] = State[Int, α]})#λ, Double, Int, String](labelling, naming)
      x(tree).transition(0)._2 must_== Bin(Leaf("1.1 is at 1"), Bin(Leaf("2.2 is at 2"), Leaf("3.3 is at 3")))
    } ^
    "measure works as expected" ! {
      val tree: BinaryTree[String] = Bin(Leaf("hello"), Bin(Leaf("world"), Bin(Leaf("erm..."), Leaf("more stuff?"))))
      val check: State[String, Unit] = State((n: String) => (n + "|", ()))
      val toLength: String => Int = _.length

      val x = Traversable.of[BinaryTree].measure[({type λ[+α] = State[String, α]})#λ, String, Int](check, toLength)
      x(tree).transition("") must_== ("||||", Bin(Leaf(5), Bin(Leaf(5), Bin(Leaf(6), Leaf(11)))))
    } ^
    "assemble" ! {
      val shape: BinaryTree[Unit] = Bin(Leaf(()), Bin(Leaf(()), Leaf(())))
      val tr = Traversable.of[BinaryTree]
      (tr.assemble(shape, List(1, 2, 3)) must_== (List(), Some(Bin(Leaf(1), Bin(Leaf(2), Leaf(3)))))) and
        (tr.assemble(shape, List(1, 2, 3, 4, 5)) must_== (List(4, 5), Some(Bin(Leaf(1), Bin(Leaf(2), Leaf(3)))))) and
        (tr.assemble(shape, List(1)) must_== (List(), None))
    } ^ end
}
