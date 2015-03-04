package statla

import statla.StatsUtil.Comoment
import spire.implicits._
import scala.Numeric.Implicits._

trait CorrelativeStats extends DescriptiveStats {
  val u: Sample
  val v: Sample

  val comoment: BigDecimal
  lazy val covariance: BigDecimal = comoment / (N - 1)
  lazy val pearson: BigDecimal = covariance / (u.stdev * v.stdev)
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

case class Corr(u: Sample, v: Sample, cm: Comoment) extends CorrelationStats {
  def +[T: Numeric](elems: (T, T)): Corr = {
    val s = elems._1.toDouble().toBigDecimal()
    val t = elems._2.toDouble().toBigDecimal()
    val (updatedU, updatedV, updatedCm) = update(s, t)
    Corr(updatedU, updatedV, updatedCm)
  }

  def ++(c2: CorrelationStats): Corr = ???
}
