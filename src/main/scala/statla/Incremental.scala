package statla

trait Incremental[S] {
  def +[T: Numeric](elem: T): S
  def ++(s2: S): S
}

trait PairIncremental[S] {
  def +[T: Numeric](elems: (T, T)): S
  def ++(s2: S): S
}
