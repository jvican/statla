import scala.util.Random
import StatsUtil._

object RandomElems {
  type SampleTest[T] = (Elems[T], SampleLike, MarginError)

  case class Elems[T: Numeric](elemToUpdate: T, elems: Seq[T]) {
    val updatedElems = elems :+ elemToUpdate
  }

  val N = 10000
  val confidence99 = marginErrorAt99(N) * N
  val confidence95 = marginErrorAt95(N) * N
  val confidence90 = marginErrorAt90(N) * N
  val defaultMarginError = confidence95

  lazy val ints: Elems[Int] =
    Elems(Random.nextInt(), Vector.fill(N)(Random.nextInt(100000)))

  lazy val doubles: Elems[Double] =
    Elems(Random.nextDouble(), Vector.fill(N)(Random.nextDouble()))

  lazy val sampleInts: SampleTest[Int] =
    (ints, Stats.parCompute(ints.elems), defaultMarginError)

  lazy val sampleDouble: SampleTest[Double] =
    (doubles, Stats.parCompute(doubles.elems), defaultMarginError)

}
