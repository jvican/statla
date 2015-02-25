import java.io.File

import org.scalatest.{Matchers, FlatSpec}

class SamplesDataSpec extends FlatSpec with Matchers {
  implicit val ds = StRD

  behavior of "A Sample"

  it should "pass all the datasets" in {
    val testFolder = new File("src/test/datasets/")
    val filepaths = testFolder.listFiles.toStream.filter(_.getName.endsWith(".dat"))

    filepaths foreach { f =>
      Datasets.read(f) match {
        case Some(res) =>
          val ((rightMean, rightStdev, _), data) = res

          println(f.getName)
          println(Statistics.mean(data))

          val sample = Stats.compute(data)

          rightMean should be (sample.mean)

          rightStdev should be (sample.stdev)

        case None => fail("Error reading and interpreting the data and certified values")
      }
    }
  }
}
