package statla

import spire.implicits._
import spire.math._
import statla.Util.numeric2Fractional

trait CorrelativeLike[T] extends PrintableStatistics {
  val covariance: T
  val pearson: T
}

abstract class CorrelativeStats[F : Fractional] extends CorrelativeLike[F] {
  require(s1.n == s2.n, "The samples you are trying to correlate does not have the same length")

  val comoment: F

  val s1: Sample[F]
  val s2: Sample[F]

  val N: Int = s1.n

  lazy val covariance: F = comoment / (N - 1)
  lazy val pearson: F = covariance / (s1.stdev * s2.stdev)

  val title = "Correlative Statistics of two samples"
  override lazy val stats = title + "\n" + delimiterOf(title) + "\n" + s"Covariance: $covariance\nPearson: $pearson" +
    "1st Sample: \n" + s1.stats + "2nd Sample: \n" + s2.stats

  def update(elems: (F, F)): (Sample[F], Sample[F], F) =
    N match {
      case 0 =>
        (s1 add elems._1, s2 add elems._2, Fractional[F].zero)
      case _ =>
        (s1 add elems._1, s2 add elems._2, comoment + (N * (elems._1 - s1.mean) * (elems._2 - s2.mean) / (N + 1)))
    }

  def combine(other: CorrelativeStats[F]): F = {
    val deltaU = s1.mean - s1.mean
    val deltaV = other.s2.mean - s2.mean
    val n1 = s1.n
    val n2 = other.s1.n
    val N = n1 + n2

    comoment + other.comoment + n1 * n2 * deltaU * deltaV / N
  }
}

class Correlation[F : Fractional](val comoment: F, val s1: Sample[F], val s2: Sample[F]) extends CorrelativeStats[F] with PairIncremental[CorrelativeStats, F] {
  private[statla] def add(elems: (F, F)): Correlation[F] = {
    val (updatedS1, updatedS2, updatedCm) = update(elems)
    new Correlation(updatedCm, updatedS1, updatedS2)
  }

  override def +[V : Numeric](elems: (V, V)): Correlation[F] =
    add((numeric2Fractional(elems._1), numeric2Fractional(elems._2)))

  override def ++(s2: CorrelativeStats[F]): Correlation[F] = ???
}

/*
trait AutocorrelationLike {
  val coefficient: BigDecimal
}

class Autocorrelation(s: Sample, cm: Comoment, lastAdded: BigDecimal) extends AutocorrelationLike with PairIncremental[AutocorrelationLike] {
  lazy val coefficient = {cm / (s).M._2;}

  override def +[T: Numeric](elems: (T, T)): Autocorrelation = {
    val updatedComoment: Comoment = s.N match {
        case 0 => 0.0
        case _ => cm + (s.N * (elems._1.toDouble().toBigDecimal() - s.mean) * (elems._2.toDouble().toBigDecimal() - s.mean) / (s.N + 1))
    }

    new Autocorrelation(s + elems._1, updatedComoment, elems._2.toDouble().toBigDecimal())
  }

  override def ++(s2: AutocorrelationLike): Autocorrelation = ???

}*/
