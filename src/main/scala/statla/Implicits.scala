package statla

import scala.language.implicitConversions
import spire.math.Fractional.BigDecimalIsFractional
import spire.math.Fractional.DoubleIsFractional

package object Implicits {
  implicit val highPrecision = BigDecimalIsFractional
  implicit val lowPrecision = DoubleIsFractional

  implicit def Seq2Sample[T, V](elems: Seq[V])(implicit t: Fractional[T], v: Numeric[V]): Sample[T] = elems.length match {
    case 0 => Utils.emptySample[T]
    case _ => Utils.compute[T, V](elems)
  }
}
