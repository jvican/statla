package statla

import scala.language.implicitConversions
import spire.math._

package object Implicits {
  implicit def Seq2Sample[T : Fractional, V : Numeric](elems: Seq[V]): Sample[T] = elems.length match {
    case 0 => Util.emptySample[T]
    case _ => Util.compute[T, V](elems)
  }
}
