object Statistics {
  import Numeric.Implicits._

  def mean[T: Numeric](elems: Seq[T]): Double = {
    elems.map(_.toDouble / elems.length).sum
  }

  def stdev[T: Numeric](elems: Seq[T]): Double = Math.sqrt(variance(elems))

  def variance[T: Numeric](elems: Seq[T]): Double = elems.length match {
    case len if len > 1 =>
      val mean = Statistics.mean(elems)
      val m2 = elems.foldLeft(0.0)((m2, e) => m2 + Math.pow(e.toDouble - mean, 2))
      m2 / (len - 1)

    case _ => 0.0
  }
}
