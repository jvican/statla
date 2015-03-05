package statla

package object Implicits {
  implicit def Seq2Sample[T: Numeric](elems: Seq[T]): Sample = elems.length match {
    case 0 => Statistics.empty
    case _ => Statistics.compute(elems)
  }
}
