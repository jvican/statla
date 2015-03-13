package statla

import statla.Utils.{CentralMoments, numeric2Fractional}

class Sample[T](val n: Int, val m: CentralMoments[T]) extends DescriptiveStats[T] with Incremental[DescriptiveStats, T] {
  override def +[V: scala.Numeric](elem: V): Sample[T] = {
    val updated = update(numeric2Fractional(elem))
    new Sample[T](updated._1, updated._2)
  }

  override def ++(s2: DescriptiveStats[T]): Sample[T] = {
    val updated = combine(s2.n, s2.m)
    new Sample(updated._1, updated._2)
  }
}
