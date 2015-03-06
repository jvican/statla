package statla

import scala.annotation.implicitNotFound

@implicitNotFound("No member of type clas Result in scope for ${T}")
trait RealLike[T] {
  def plus(x: T, y: T): T
  def minus(x: T, y: T): T
  def times(x: T, y: T): T
  def divide(x: T, y: T): T
  def pow[S: Numeric](x: T, to: S): T

}

object RealLike {

  implicit object Precise extends RealLike[BigDecimal] {

  }

  implicit object Imprecise extends RealLike[Double] {
  }
}
