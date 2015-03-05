package statla

import statla.StatsUtil.CentralMoments
import spire.implicits._
import scala.Numeric.Implicits._

case class Sample(N: Int, M: CentralMoments) extends Incremental[DescriptiveStats] with DescriptiveStats {
  override def +[T: Numeric](elem: T): Sample = {
    val updated = update(elem.toDouble().toBigDecimal())
    Sample(updated._1, updated._2)
  }

  override def ++(s2: DescriptiveStats): Sample = {
    val updated = combine(s2.N, s2.M)
    Sample(updated._1, updated._2)
  }
}
