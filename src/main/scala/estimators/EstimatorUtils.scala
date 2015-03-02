package estimators

object EstimatorUtils {
  def timesSatisfied[T: Numeric](elems: Seq[T], cond: T => Boolean): Int =
    elems.count(cond)
}
