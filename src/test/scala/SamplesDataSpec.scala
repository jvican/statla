import java.io.File

import org.scalatest.{FlatSpec, Matchers}

class SamplesDataSpec extends FlatSpec with Matchers {
  implicit val ds = StRD

  // Be careful, the order of the parameters matter, the first one is the reference to round off
  def roundOff(first: BigDecimal, second: BigDecimal): BigDecimal = {
    import scala.math.BigDecimal.RoundingMode._
    second.setScale(first.scale, HALF_UP)
  }

  behavior of "A Sample"

  it should "pass all the datasets" in {
    val testFolder = new File("src/test/datasets/")
    val dataFiles = testFolder.listFiles.toStream.filter(_.getName.endsWith(".dat"))

    dataFiles foreach { f =>
      Datasets.read(f) match {
        case Some(res) =>
          val ((rightMean, rightStdev, _), data) = res

          println(f.getName)

          val sample = Stats.compute(data)
          rightMean should be (roundOff(rightMean, sample.mean))
          rightStdev should be (roundOff(rightStdev, sample.stdev))

        case None => fail("Error reading and interpreting the data and certified values")
      }
    }
  }
}
