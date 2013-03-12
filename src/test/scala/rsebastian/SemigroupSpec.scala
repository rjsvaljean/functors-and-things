package rsebastian

import org.specs2.Specification

class SemigroupSpec extends Specification { def is =
  "can create new semigroup instances from existing ones" ! {
    import rsebastian.Semigroup.SemiGroupInt
    case class Percentage(n: BigDecimal)
    val nToP: BigDecimal => Percentage = Percentage.apply _
    val pToN: Percentage => BigDecimal = _.n
    val intToBigdecimal: Int => BigDecimal = BigDecimal(_)

    val bigDecimalSemigroup = implicitly[Semigroup[Int]].smap(intToBigdecimal, {b: BigDecimal => b.toInt})
    bigDecimalSemigroup.smap(nToP, pToN).append(Percentage(BigDecimal("1")), Percentage(BigDecimal("2"))) must_== Percentage(BigDecimal("3"))
  }
}
