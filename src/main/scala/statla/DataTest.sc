// Usage example

import statla.Implicits._
import scala.util.Random
val v = Vector.fill[Int](100)(Math.abs(Random.nextInt()))
println(Seq2Sample[Double, Int](v).stats)
