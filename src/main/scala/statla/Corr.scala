package statla

import statla.StatsUtil._
import spire.implicits._
import scala.Numeric.Implicits._

case class Corr(u: Sample, v: Sample, cm: Comoment) extends CorrelationStats {
  def +[T: Numeric](elems: (T, T)): Corr = {
    val s = elems._1.toDouble().toBigDecimal()
    val t = elems._2.toDouble().toBigDecimal()
    val (updatedU, updatedV, updatedCm) = update(s, t)
    Corr(updatedU, updatedV, updatedCm)
  }

  def ++(c2: CorrelationStats): Corr = ???
}
