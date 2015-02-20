object Statistics {
  import Numeric.Implicits._

  /**
   * Arithmetic mean of a sequence of elements.
   * @param elems Sequence of elements of any Numeric type
   *
   * @return Arithmetic mean of value Double
   *
   */
  def mean[T: Numeric](elems: Seq[T]): Double =
    elems.sum.toDouble / elems.length

  /**
   * Incremental implementation of the Arithmetic mean of a sequence of elements. This is useful
   * to compute the mean with the last known mean and a new value.
   *
   * @param lastMean Last known mean of lastN elements
   * @param lastN Last known number of elements
   * @param elem Numeric element to update the mean and be included in the sample
   *
   * @return Arithmetic mean of value Double
   *
   */
  def updateMean[T: Numeric](lastMean: Double, lastN: Int, elem: T): Double =
    (lastMean * lastN + elem.toDouble) / (lastN + 1)

  def stdev[T: Numeric](elems: Seq[T]): Double = Math.sqrt(variance(elems))

  /**
   * Computes an incremental standard deviation
   *
   * */
  def updateStdev[T: Numeric](lastStdev: Double, lastMean: Double, lastN: Int, elem: T): Double =
    Math.sqrt(updateVariance(lastStdev*lastStdev, lastMean, lastN, elem))

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

  /**
   * Computes an updated sample variance from the current mean and a desired elem Numeric. Specification of the
   * incremental algorithm:
   * [[http://en.wikipedia.org/wiki/Algorithms_for_calculating_variance Variance formula in terms of previous values]]
   *
   * @param lastVariance Last known variance of lastN numbers
   * @param lastMean Last known mean of lastN numbers
   * @param lastN Number of elements in the last sample
   * @param elem Numeric element to update the variance and be included in the sample
   *
   * @return An updated variance
   */

  def updateVariance[T: Numeric](lastVariance: Double, lastMean: Double, lastN: Int, elem: T): Double = {
    val first = (lastN - 1) * lastVariance / lastN
    val second = ((elem.toDouble - lastMean) * (elem.toDouble - lastMean)) / (lastN + 1)
    first + second
  }
}
