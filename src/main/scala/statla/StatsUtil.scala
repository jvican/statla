package statla

package object StatsUtil {
  type CentralMoments = (BigDecimal, BigDecimal, BigDecimal, BigDecimal) // M1, M2, M3, M4
  type Comoment = BigDecimal
  type StatsCore = (Int, CentralMoments)

  val zeroMoments: CentralMoments = (0.0, 0.0, 0.0, 0.0)

  // Be careful, the order of the parameters matter, the first one is the reference to round off
  def roundToLowerScale(first: BigDecimal, second: BigDecimal): BigDecimal = {
    import scala.math.BigDecimal.RoundingMode._
    second.setScale(first.scale, HALF_UP)
  }

}
