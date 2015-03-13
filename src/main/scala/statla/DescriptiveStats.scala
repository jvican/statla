package statla

import spire.implicits._
import spire.math._
import statla.Utils.CentralMoments

trait DescriptiveLike[T] extends PrintableStatistics {
  val mean: T
  val variance: T
  val stdev: T
  val skewness: T
  val kurtosis: T
}

abstract class DescriptiveStats[@specialized(Float, Double) T : Fractional] extends DescriptiveLike[T] {
  val m: CentralMoments[T]
  val n: Int
  
  lazy val mean = m._1
  lazy val variance = m._2 / (n - 1)
  lazy val stdev = variance.sqrt()
  lazy val skewness = m._3 * n.sqrt() / (m._2 ** 1.5)
  lazy val kurtosis = n * m._4 / (m._2 * m._2) - 3.0

  val title = "Descriptive Statistics"
  lazy val stats: String = title + "\n" + delimiterOf(title) + "\n" +
    s"Mean: $mean\nVariance: $variance\nStandard deviation: $stdev\nSkewness: $skewness\nKurtosis: $kurtosis\n"
  
  protected def update(elem: T): (Int, CentralMoments[T]) = {
    val updatedN = n + 1
    val delta = elem - m._1
    val deltaM1 = delta / updatedN
    val deltaM1M1 = deltaM1 * deltaM1
    val t1 = delta * deltaM1 * n

    (n + 1) -> (
      m._1 + deltaM1,
      m._2 + t1,
      m._3 + t1 * deltaM1 * (updatedN - 2) - 3 * deltaM1 * m._2,
      m._4 + t1 * deltaM1M1 * (updatedN * updatedN - 3 * updatedN + 3)
        + 6 * deltaM1M1 * m._2
      )
  }

  protected def combine(other: DescriptiveStats): (Int, CentralMoments[T]) = combine(other.n, other.m)

  protected def combine(n2: Int, m2: CentralMoments[T]): (Int, CentralMoments[T]) = {
    val N = n + n2
    val NN = N * N
    val n1n1 = n * n
    val n2n2 = n2 * n2
    val n1n2 = n * n2
    val delta = m2._1 - m._1
    val delta2 = delta * delta

    N -> (
      (n * m._1 + n2 * m2._1) / N,
      m._2 + m2._2 + (delta2 * n1n2 / N),
      m._3 + m2._3 + (delta2 * delta) * n1n2 * (n - n2) / NN
        + 3 * delta * (n * m2._2 - n2 * m._2),
      m._4 + m2._4 + (delta2 * delta2) * n1n2 * (n1n1 - n1n2 + n2n2) / (NN * N)
        + 6 * delta2 * (n1n1 * m2._2 + n2n2 * m._2) / NN
        + 4 * delta * (n * m2._3 - n2 * m._3) / N
      )
  }
}
