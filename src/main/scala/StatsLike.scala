import StatsUtil._
import Numeric.Implicits._

/**
 * This class is an extension of __Knuth__ and __Welford__ algorithm for computing standard
 * deviation in __one__ pass (not the two-pass method computing mean and stdev separately). This
 * allows to compute statistics in an incremental way, avoiding to compute these functions
 * every time we add an element to our current sample. This algorithm is numerically stable,
 * avoiding the well known problem of loss of significance and inaccuracy, as well as efficient.
 * Moreover, it can be parallelized.
 *
 * 
 *
 *
 * */

package object StatsUtil {
  type CentralMoments = (Double, Double, Double, Double) // M1, M2, M3, M4
  type MarginError = Double

  val zeroMoments: CentralMoments = (0.0, 0.0, 0.0, 0.0)

  def marginErrorAt90(N: Int): MarginError =
    0.82 / Math.sqrt(N)

  def marginErrorAt95(N: Int): MarginError =
    0.98 / Math.sqrt(N)

  def marginErrorAt99(N: Int): MarginError =
    1.29 / Math.sqrt(N)
}

trait StatsLike {
  val N: Int
  val M: CentralMoments

  lazy val mean: Double = M._1
  lazy val variance: Double = M._2 / (N - 1)
  lazy val stdev: Double = Math.sqrt(variance)
  lazy val skewness: Double = Math.sqrt(N) * M._3 / Math.pow(M._2, 1.5)
  lazy val kurtosis: Double = N * M._4 / (M._2 * M._2) - 3.0
}

trait SampleLike extends StatsLike {
  def +[T: Numeric](elem: T): SampleLike
  def ++(s2: SampleLike): SampleLike
}

case class Sample(N: Int, M: CentralMoments) extends SampleLike {
  def +[T: Numeric](elem: T): Sample = {
    val updated = Stats.update(N, M)(elem)
    Sample(updated._1, updated._2)
  }

  def ++(s2: SampleLike): Sample = {
    val updated = Stats.combine(N, M)(s2.N, s2.M)
    Sample(updated._1, updated._2)
  }
}

object Stats {
  val empty = Sample(0, zeroMoments)

  def compute[T: Numeric](elems: Seq[T]): SampleLike =
    elems.foldLeft(empty)(_ + _)

  def parCompute[T: Numeric](elems: Seq[T]): SampleLike =
    elems.par.foldLeft(empty)(_ + _)

  def update[T: Numeric](n: Int, m: CentralMoments)(elem: T): (Int, CentralMoments) = {
    val N = n + 1
    val delta = elem.toDouble() - m._1
    val deltaM1 = delta / N
    val deltaM1M1 = deltaM1 * deltaM1
    val t1 = delta * deltaM1 * n

    (n + 1) -> (
     m._1 + deltaM1,
     m._2 + t1,
     m._3 + t1 * deltaM1 * (N - 2) - 3 * deltaM1 * m._2,
     m._4 + t1 * deltaM1M1 * (N * N - 3 * N + 3)
       + 6 * deltaM1M1 * m._2
    )
  }

  def combine(n1: Int, m1: CentralMoments)(n2: Int, m2: CentralMoments): (Int, CentralMoments) = {
    val n = n1 + n2
    val nn = n * n
    val n1n1 = n1 * n1
    val n2n2 = n2 * n2
    val n1n2 = n1 * n2
    val delta = m2._1 - m1._1
    val delta2 = delta * delta

    n -> (
      (n1 * m1._1 + n2 * m2._1) / n,
      m1._2 + m2._2 + (delta2 * n1n2 / n),
      m1._3 + m2._3 + (delta2 * delta) * n1n2 * (n1 - n2) / nn
        + 3 * delta * (n1 * m2._2 - n2 * m1._2),
      m1._4 + m2._4 + (delta2 * delta2) * n1n2 * (n1n1 - n1n2 + n2n2) / (nn * n)
        + 6 * delta2 * (n1n1 * m2._2 + n2n2 * m1._2) / nn
        + 4 * delta * (n1 * m2._3 - n2 * m1._3) / n
    )
  }
}

