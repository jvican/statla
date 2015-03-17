package statla

import spire.math.{Fractional, Numeric}
import statla.Util.{CentralMoments, numeric2Fractional}

class Sample[F : Fractional](val n: Int, val m: CentralMoments[F]) extends DescriptiveStats[F] with Incremental[DescriptiveStats, F] {
  protected def add(elem: F): Sample[F] = {
    val updated = update(elem)
    new Sample[F](updated._1, updated._2)
  }

  override def +[V : Numeric](elem: V): Sample[F] =
    add(numeric2Fractional(elem))

  override def ++(s2: DescriptiveStats[F]): Sample[F] = {
    val updated = combine(s2.n, s2.m)
    new Sample(updated._1, updated._2)
  }
}
