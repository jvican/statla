package statla

import scala.language.higherKinds

trait Incremental[S[_], T] {
  def +[V : Numeric](elem: V): S[T]
  def ++(s2: S[T]): S[T]
}

trait PairIncremental[S[_], T] {
  def +[V : Numeric](elems: (V, V)): S[T]
  def ++(s2: S[T]): S[T]
}
