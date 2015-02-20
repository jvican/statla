import scala.language.implicitConversions

package object Statla {
  implicit def Seq2Sample[T: Numeric](elems: Seq[T]): SampleLike = elems.length match {
    case 0 => Stats.empty
    case _ => Stats.compute(elems)
  }
}
