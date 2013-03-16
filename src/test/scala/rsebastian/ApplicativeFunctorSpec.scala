package rsebastian

import org.specs2.Specification
import rsebastian.OptionHelpers._
import rsebastian.WhyApplicativeFunctor.{sequence, validate}
import rsebastian.ApplicativeFunctor._
import rsebastian.Monad._
import org.specs2.matcher.DataTables


class ApplicativeFunctorSpec extends Specification with DataTables {
  implicit def ValidationApp = new ValidationApplicativeFunctor[Int]()
  val OptionApplicativeFunctor: ApplicativeFunctor[Option] = implicitly[ApplicativeFunctor[Option]]
  def pureId[A] = IdentApplicativeFunctor.pure[A]
  def appId[A, B] = IdentApplicativeFunctor.apply[A, B] _
  def pureOpt[A] = OptionApplicativeFunctor.pure[A]
  def appOpt[A, B] = implicitly[ApplicativeFunctor[Option]].apply[A, B] _
  def pureVal[A] = ValidationApp.pure[A]
  def appVal[A, B] = ValidationApp.apply[A, B] _
  def toInt: Double => Int = _.toInt
  def toDouble: String => Double = _.toDouble

  def is =
  "Applicative laws: " ^
    "Identity" ! {
      (appId(pureId(identity[Int] _))(pureId(1)) must_== pureId(1)) and
      (appOpt(pureOpt(identity[Int] _))(pureOpt(1)) must_== pureOpt(1)) and
      (appVal(pureVal(identity[Int] _))(pureVal(1)) must_== pureVal(1))
    } ^
    "Composition" ! {
      appOpt(pureOpt(toInt))(appOpt(pureOpt(toDouble))(pureOpt("1.1"))).must_==(
        appOpt(pureOpt(toDouble andThen toInt))(pureOpt("1.1"))
      )
    } ^
    "Homomorphism" ! {
      appVal(pureVal(toInt))(pureVal(1.1)) must_== pureVal(toInt(1.1))
    } ^
    "Interchange" ! {
      val fab: Option[(Double) => Int] = pureOpt(toInt)
      appOpt(fab)(pureOpt(1.1)) must_== appOpt(pureOpt((f: Double => Int) => f(1.1)))(fab)
    } ^ bt ^
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
  "Applicative functors compose" ! {
    implicitly[ApplicativeFunctor[List]].compose(OptionApplicativeFunctor).apply[String, Int](
      List(Some(_.length), None)
    )(List(Some("hello"), Some("rat"))).must_==(
      List(Some(5), Some(3), None, None))
  }

}
