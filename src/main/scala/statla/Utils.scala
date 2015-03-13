package statla

import spire.math.Fractional
import spire.implicits._

package object Utils {
  type CentralMoments[T] = (T, T, T, T)
  type Comoment = BigDecimal

  def zero[T : Fractional] = Fractional[T].zero
  def zeroMoments[T : Fractional] = (Fractional[T].zero, Fractional[T].zero, Fractional[T].zero, Fractional[T].zero)

  def emptySample[T : Fractional] = new Sample[T](0, zeroMoments[T])
  def emptyCorrelation[T : Fractional] = new Correlation[T](zero, emptySample, emptySample)
  
  def numeric2Fractional[N, F](elem: N) = {
    import spire.math.Numeric
    Numeric[F].fromType[N](elem)
  }

  // Be careful, the order of the parameters matter, the first one is the reference to round off
  def roundToLowerScale(first: BigDecimal, second: BigDecimal): BigDecimal = {
    import scala.math.BigDecimal.RoundingMode._
    second.setScale(first.scale, HALF_UP)
  }
  
  def compute[T : Fractional, V : Numeric](elems: Seq[V]): Sample[T] =
    elems.foldLeft(emptySample[T])(_ + _)

  def parCompute[T : Fractional, V : Numeric](elems: Seq[V]): Sample[T] =
    elems.par.foldLeft(emptySample[T])(_ + _)

  def biasedCovariance(cv: BigDecimal, N: Int): BigDecimal =
    cv * (N - 1) / N

  def autocorrelationCoefficient[V : Numeric](elems: Seq[V], mean: BigDecimal, stdev: BigDecimal): BigDecimal =
    biasedCovariance(rawCovariance(elems.init, elems.tail, mean, mean), elems.length - 1) / (stdev * stdev)

  // temporary function
  def rawCovariance[T : Fractional, V : Numeric](elems1: Seq[V], elems2: Seq[V], uMean: T, vMean: T): T =
    elems1.zip(elems2).map((es: (V, V)) => (numeric2Fractional[V, T](es._1) - uMean) * (numeric2Fractional[V, T](es._2) - vMean) / (elems1.length - 1)).sum

  def computeCorrelation[T : Fractional, V : Numeric](elems1: Seq[V], elems2: Seq[V]): Correlation[T] =
    elems1.zip(elems2).foldLeft(emptyCorrelation[T])(_ + _)

}
