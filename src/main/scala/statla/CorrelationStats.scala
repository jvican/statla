package statla

import statla.StatsUtil.Comoment
import spire.implicits._
import scala.Numeric.Implicits._

trait CorrelativeLike {
  val covariance: BigDecimal
  val pearson: BigDecimal
}

trait CorrelativeStats {
  //require(s1.N == s2.N, "The samples you are trying to correlate does not have the same length")
  val s1: Sample
  val s2: Sample

  val N: Int = s1.N

  val cm: Comoment

  lazy val covariance: BigDecimal = cm / (N - 1)
  lazy val pearson: BigDecimal = covariance / (s1.stdev * s2.stdev)

  def update(elems: (BigDecimal, BigDecimal)): (Sample, Sample, Comoment) =
    N match {
      case 0 =>
        (s1 + elems._1, s2 + elems._2, 0.0)
      case _ =>
        (s1 + elems._1, s2 + elems._2, cm + (N * (elems._1 - s1.mean) * (elems._2 - s2.mean) / (N + 1)))
    }

  def combine(cs: CorrelativeStats): Comoment = {
    val deltaU = s1.mean - s1.mean
    val deltaV = cs.s2.mean - s2.mean
    val n1 = s1.N
    val n2 = cs.s1.N
    val N = n1 + n2

    cm + cs.cm + n1 * n2 * deltaU * deltaV / N
  }
}

case class Correlation(s1: Sample, s2: Sample, cm: Comoment) extends CorrelativeStats with PairIncremental[CorrelativeStats] {
  override def +[T: Numeric](elems: (T, T)): Correlation = {
    val (s, t) = (elems._1.toDouble().toBigDecimal(), elems._2.toDouble().toBigDecimal())
    val (updatedS1, updatedS2, updatedCm) = update((s, t))
    Correlation(updatedS1, updatedS2, updatedCm)
  }

  override def ++(s2: CorrelativeStats): Correlation = ???
}

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

}
