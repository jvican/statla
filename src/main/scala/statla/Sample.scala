package statla

import statla.StatsUtil.CentralMoments
import spire.implicits._
import scala.Numeric.Implicits._

trait SampleLike extends DescriptiveStats {
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
