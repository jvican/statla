package statla

package object Implicits {
  implicit def Seq2Sample[T: Numeric](elems: Seq[T]): Sample = elems.length match {
    case 0 => Statistics.empty
    case _ => Statistics.compute(elems)
  }

/*  implicit def Numeric2BigDecimal[T: Numeric](elem: T): BigDecimal =
    elem.toDouble().toBigDecimal()

  implicit def Numeric2BigDecimalPair[T: Numeric](elems: (T, T)): (BigDecimal, BigDecimal) = {
    val (s, t) = (elems._1, elems._2)
    (s, t)
  }*/
}
