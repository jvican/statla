// Usage example

import statla.Util._
import scala.util.Random

val v = Vector.fill[Int](100)(Math.abs(Random.nextInt()))
println(compute[Double, Int](v).stats) // Fast
println(compute[BigDecimal, Int](v).stats) // High precision
