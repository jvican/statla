object Statistics {
  import Numeric.Implicits._
  import spire.implicits._

  def mean[T: Numeric](elems: Seq[T]): BigDecimal = {
    elems.map(_.toDouble().toBigDecimal()).sum / BigDecimal(elems.length)
  }
/*
  def stdev[T: Numeric](elems: Seq[T]): Double = Math.sqrt(variance(elems))

  def variance[T: Numeric](elems: Seq[T]): BigDecimal = elems.length match {
    case len if len > 1 =>
      val mean = Statistics.mean(elems)
      val m2 = elems.foldLeft[BigDecimal](BigDecimal(0.0)((m2: BigDecimal, e: T) => m2 + (e.toBigDecimal() - mean).pow(2))
      m2 / (len - 1)

    case _ => 0.0
  }*/
}
