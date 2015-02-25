import java.io.File

import org.scalatest.{FlatSpec, Matchers}

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
          val ((rightMean: CertifiedValue, rightStdev: CertifiedValue, _), data) = res

          val sample = Stats.compute(data)
          sample.mean should matchWithCertifiedValue (rightMean)
          sample.stdev should matchWithCertifiedValue (rightStdev)

        case None => fail("Error reading and interpreting the data and certified values")
      }
    }
  }
}
