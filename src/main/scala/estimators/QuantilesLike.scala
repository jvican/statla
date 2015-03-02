package estimators

import estimators.EstimatorUtils._
import spire.implicits._

import scala.Numeric.Implicits._

trait EWSA {
  val p: Double // p-quantile
  val quantile: BigDecimal
  val density: BigDecimal
  val neighborhood: BigDecimal

  val w: BigDecimal = 0.05

  protected def quantileCond[T: Numeric](elem: T): Boolean =
    elem.toDouble().toBigDecimal() <= quantile

  protected def densityCond[T: Numeric](elem: T): Boolean =
    (elem.toDouble().toBigDecimal() - quantile).abs <= neighborhood

  protected def updateNeighborhood[T: Numeric](elems: Seq[T]): BigDecimal = ???
  
  protected def updateQuantile[T: Numeric](elems: Seq[T]): BigDecimal =
    quantile + (w / density ) * (p - (timesSatisfied(elems, quantileCond[T]) / elems.length))

  protected def updateDensity[T: Numeric](elems: Seq[T]): BigDecimal =
    (1 - w) * density + w / (2 * neighborhood * elems.length) * timesSatisfied(elems, densityCond[T])

}

case class Quantile(p: Double, quantile: BigDecimal, density: BigDecimal, neighborhood: BigDecimal) extends EWSA {
  def update[T: Numeric](elems: Seq[T]): Quantile =
    Quantile(
      p,
      updateQuantile(elems),
      updateDensity(elems),
      updateNeighborhood(elems)
    )

  def get: (Double, BigDecimal) = (p, quantile)
}

case class Quantiles(ps: Seq[Quantile]) {
  def update[T: Numeric](elems: Seq[T]): Quantiles =
    Quantiles(
      ps map (_ update elems)
    )

  def get: Seq[(Double, BigDecimal)] =
    ps map (_.get)
}

object Quantiles {

/*  def estimate[T: Numeric](elems: Seq[T], ps: Seq[Double]): Quantiles = {
    val sortedElems = elems.sorted

    def quantile(p: Double): BigDecimal =
      sortedElems.apply(p.ceil.toInt).toDouble().toBigDecimal()

    val M = elems.length

    for {
      p <- ps
      firstEstimate <- quantile(p)
      iqr: BigDecimal <- quantile(0.75) - quantile(0.25)
      neighborhood <- iqr * (1 / M) * elems.foldLeft[BigDecimal](0.0)(_ + _.toDouble().toBigDecimal().pow(-0.5))
      density <- (1 / (2 * neighborhood * M)) * max(timesSatisfied(elems, densityCond), 1)
    }
    val firstEstimate = quantile()
    val iqr


  }*/



}

