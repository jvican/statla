object Statistics {
  import Numeric.Implicits._

  /**
   * Arithmetic mean of a sequence of elements.
   * @param elems Sequence of elements of any Numeric type
   *
   * @return Arithmetic mean of value Double
   *
   */
  def mean[T: Numeric](elems: Seq[T]): Double = {
    elems.map(_.toDouble / elems.length).sum
  }

  def mean2[T: Numeric](elems: Seq[T]): Double =
    elems.sum.toDouble / elems.length

  def mean3[T: Numeric](elems: Seq[T]): Double = {
    elems.grouped(100).map(e => e.sum.toDouble / e.length).sum
  }

  def stdev[T: Numeric](elems: Seq[T]): Double = Math.sqrt(variance(elems))

  /**
   * Computes the sample variance (unbiased) of N elements of type Numeric
   *
   * @param elems Sequence of elements of any Numeric type
   *
   * @return The sample variance of a sample
   */
  def variance[T: Numeric](elems: Seq[T]): Double = elems.length match {
    case len if len > 1 =>
      val mean = Statistics.mean(elems)
      val m2 = elems.foldLeft(0.0)((m2, e) => m2 + Math.pow(e.toDouble - mean, 2))
      m2 / (len - 1)

    case _ => 0.0
  }
}
