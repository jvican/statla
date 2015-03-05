package statla

import statla.StatsUtil.zeroMoments
import spire.implicits._
import scala.Numeric.Implicits._

object Statistics {
  val empty: Sample = Sample(0, zeroMoments)
  val corrEmpty: Correlation = Correlation(empty, empty, 0.0)

  def compute[T: Numeric](elems: Seq[T]): Sample =
    elems.foldLeft(empty)(_ + _)

  def parCompute[T: Numeric](elems: Seq[T]): Sample =
    elems.par.foldLeft(empty)(_ + _)

  def computeAutocorrelation[T: Numeric](elems: Seq[T]): CorrelativeStats =
    computeCorrelation[T](elems.init, elems.tail)

  def biasedCovariance(cv: BigDecimal, N: Int): BigDecimal =
    cv * (N - 1) / N

  def autocorrelationCoefficient[T: Numeric](elems: Seq[T], mean: BigDecimal, stdev: BigDecimal): BigDecimal =
    biasedCovariance(cov(elems.init, elems.tail, mean, mean), elems.length - 1) / (stdev * stdev)

  // temporary function
  def cov[T: Numeric](elems1: Seq[T], elems2: Seq[T], uMean: BigDecimal, vMean: BigDecimal): BigDecimal =
    elems1.zip(elems2).map((es: (T, T)) => (es._1.toDouble().toBigDecimal() - uMean) * (es._2.toDouble().toBigDecimal() - vMean) / (elems1.length - 1)).sum

  def computeCorrelation[T: Numeric](elems1: Seq[T], elems2: Seq[T]): Correlation =
    elems1.zip(elems2).foldLeft(corrEmpty)(_ + _)
}
