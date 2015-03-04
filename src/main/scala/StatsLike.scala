import StatsUtil._
import spire.implicits._

import scala.Numeric.Implicits._

package object StatsUtil {
  type CentralMoments = (BigDecimal, BigDecimal, BigDecimal, BigDecimal) // M1, M2, M3, M4
  type Comoment = BigDecimal
  type StatsCore = (Int, CentralMoments)

  val zeroMoments: CentralMoments = (0.0, 0.0, 0.0, 0.0)

  // Be careful, the order of the parameters matter, the first one is the reference to round off
  def roundToLowerScale(first: BigDecimal, second: BigDecimal): BigDecimal = {
    import scala.math.BigDecimal.RoundingMode._
    second.setScale(first.scale, HALF_UP)
  }

}

trait StatsLike {
  val N: Int
  val M: CentralMoments

  lazy val mean: BigDecimal = M._1
  lazy val variance: BigDecimal = M._2 / (N - 1)
  lazy val biasedVariance: BigDecimal = M._2 / N
  lazy val stdev: BigDecimal = variance.sqrt()
  lazy val biasedStdev: BigDecimal = biasedVariance.sqrt()
  lazy val skewness: BigDecimal = BigDecimal(N).sqrt() * M._3 / M._2.fpow(1.5)
  lazy val kurtosis: BigDecimal = N * M._4 / (M._2 * M._2) - 3.0

  lazy val stats: String = "Descriptive statistics\n======================\n" +
    s"Mean: $mean\nVariance: $variance\nStandard deviation: $stdev\nSkewness: $skewness\nKurtosis: $kurtosis"

  def update(elem: BigDecimal): StatsCore = {
    val updatedN = N + 1
    val delta = elem - M._1
    val deltaM1 = delta / updatedN
    val deltaM1M1 = deltaM1 * deltaM1
    val t1 = delta * deltaM1 * N

    (N + 1) -> (
      M._1 + deltaM1,
      M._2 + t1,
      M._3 + t1 * deltaM1 * (updatedN - 2) - 3 * deltaM1 * M._2,
      M._4 + t1 * deltaM1M1 * (updatedN * updatedN - 3 * updatedN + 3)
        + 6 * deltaM1M1 * M._2
      )
  }
  
  def combine(s: StatsLike): StatsCore = combine(s.N, s.M)
  
  def combine(N2: Int, M2: CentralMoments): StatsCore = {
    val nbd1 = N.toBigDecimal()
    val nbd2 = N2.toBigDecimal()
    val n = nbd1 + nbd2
    val nn = n * n
    val n1n1 = nbd1 * nbd1
    val n2n2 = nbd2 * nbd2
    val n1n2 = nbd1 * nbd2
    val delta = M2._1 - M._1
    val delta2 = delta * delta

    n.toIntExact -> (
      (N * M._1 + N2 * M2._1) / n,
      M._2 + M2._2 + (delta2 * n1n2 / n),
      M._3 + M2._3 + (delta2 * delta) * n1n2 * (N - N2) / nn
        + 3 * delta * (N * M2._2 - N2 * M._2),
      M._4 + M2._4 + (delta2 * delta2) * n1n2 * (n1n1 - n1n2 + n2n2) / (nn * n)
        + 6 * delta2 * (n1n1 * M2._2 + n2n2 * M._2) / nn
        + 4 * delta * (N * M2._3 - N2 * M._3) / n
      )
  }
}

trait CorrelationStats {
  val u: Sample
  val v: Sample

  val N: Int = u.N

  val cm: Comoment
  
  lazy val covariance: BigDecimal = cm / (N - 1)
  lazy val pearson: BigDecimal = covariance / (u.stdev * v.stdev)
  
  def update(elems: (BigDecimal, BigDecimal)): (Sample, Sample, Comoment) = {
    val (s, t) = elems
    val s1 = u + s
    val s2 = v + t

    N match {
      case 0 =>
        (s1, s2, 0.0)
      case _ =>
        (s1, s2, cm + (N * (s - u.mean) * (t - v.mean) / (N + 1)))
    }

  }

  def combine(cs: CorrelationStats): Comoment = {
    val deltaU = u.mean - u.mean
    val deltaV = cs.v.mean - v.mean
    val n1 = u.N
    val n2 = cs.u.N
    val N = n1 + n2

    cm + cs.cm + n1 * n2 * deltaU * deltaV / N
  }
}

trait SampleLike extends StatsLike {
  def +[T: Numeric](elem: T): SampleLike
  def ++(s2: SampleLike): SampleLike
}

case class Sample(N: Int, M: CentralMoments) extends SampleLike {
  def +[T: Numeric](elem: T): Sample = {
    val updated = update(elem.toDouble().toBigDecimal())
    Sample(updated._1, updated._2)
  }
  
  def ++(s2: SampleLike): Sample = {
    val updated = combine(s2.N, s2.M)
    Sample(updated._1, updated._2)
  }
}

case class Corr(u: Sample, v: Sample, cm: Comoment) extends CorrelationStats {
  def +[T: Numeric](elems: (T, T)): Corr = {
    val s = elems._1.toDouble().toBigDecimal()
    val t = elems._2.toDouble().toBigDecimal()
    val (updatedU, updatedV, updatedCm) = update(s, t)
    Corr(updatedU, updatedV, updatedCm)
  }

  def ++(c2: CorrelationStats): Corr = ???
}

object Stats {
  val empty: Sample = Sample(0, zeroMoments)
  val corrEmpty: Corr = Corr(empty, empty, 0.0)

  def compute[T: Numeric](elems: Seq[T]): Sample =
    elems.foldLeft(empty)(_ + _)

  def parCompute[T: Numeric](elems: Seq[T]): Sample =
    elems.par.foldLeft(empty)(_ + _)

  def computeAutocorrelation[T: Numeric](elems: Seq[T]): CorrelationStats =
    computeCorrelation[T](elems.init, elems.tail)

  def biasedCovariance(cv: BigDecimal, N: Int): BigDecimal =
    cv * (N - 1) / N

  def autocorrelationCoefficient[T: Numeric](elems: Seq[T], mean: BigDecimal, stdev: BigDecimal): BigDecimal =
    biasedCovariance(cov(elems.init, elems.tail, mean, mean), elems.length - 1) / (stdev * stdev)

  // temporary function
  def cov[T: Numeric](elems1: Seq[T], elems2: Seq[T], uMean: BigDecimal, vMean: BigDecimal): BigDecimal =
    elems1.zip(elems2).map((es: (T, T)) => (es._1.toDouble().toBigDecimal() - uMean) * (es._2.toDouble().toBigDecimal() - vMean) / (elems1.length - 1)).reduce(_ + _)

  def computeCorrelation[T: Numeric](elems1: Seq[T], elems2: Seq[T]): CorrelationStats =
    elems1.zip(elems2).foldLeft(corrEmpty)((ce: Corr, elems: (T, T)) => ce + (elems._1, elems._2))
}

