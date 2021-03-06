import java.io.File

import org.scalatest.{FlatSpec, Matchers}
import statla.Util

class SamplesDataSpec extends FlatSpec with Matchers {
  implicit val ds = StRD

  behavior of "A Sample"

  it should "pass all the datasets" in {
    import DatasetMatcher._
    import Datasets.CertifiedValue

    val testFolder = new File("src/test/datasets/")
    val dataFiles = testFolder.listFiles.toStream.filter(_.getName.endsWith(".dat"))

    dataFiles foreach { f =>
      Datasets.read(f) match {
        case Some(res) =>
          val ((correctMean: CertifiedValue, correctStdev: CertifiedValue, correctAutocorr: CertifiedValue), data) = res

          val sample = Util.compute[BigDecimal, BigDecimal](data)
          sample.mean should matchWithCertifiedValue (correctMean)
          sample.stdev should matchWithCertifiedValue (correctStdev)

          // TODO AUTOCORRELATION
          //val pearson = Utils.autocorrelationCoefficient(data, sample.mean, sample.stdev)
          //val autocorr = Utils.autocorrelateByOne(data)
          //pearson should matchWithCertifiedValue (correctAutocorr)

        case None => fail("Error reading and interpreting the data and certified values")
      }
    }
  }
}
