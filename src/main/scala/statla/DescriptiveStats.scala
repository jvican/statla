package statla

import statla.StatsUtil.{StatsCore, CentralMoments}
import spire.implicits._
import scala.Numeric.Implicits._

trait DescriptiveStats {
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

  def combine(s: DescriptiveStats): StatsCore = combine(s.N, s.M)

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
