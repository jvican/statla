import java.io.File

import scala.io.Source
import scala.language.postfixOps
import scala.util.matching.Regex
import spire.implicits._

trait DataSet {
  val HeaderLineKeyword: String
  val DataLineKeyword: String
  val CertifiedValuesKeyword: String
  val ValueSplitter: Char
  val LinesRegex: Regex
  val ExactSymbol: String
}

object StRD extends DataSet {
  override val HeaderLineKeyword: String = "Header"
  override val DataLineKeyword: String = "Data"
  override val CertifiedValuesKeyword: String = "Certified Values"
  override val ValueSplitter: Char = ':'
  override val LinesRegex = """\d+\s+to\s+\d+""".r
  override val ExactSymbol = "exact"
}

object Datasets {
  type LinesInFile = (Int, Int)

  type CertifiedValue = (BigDecimal, Boolean)
  type CertifiedValues = (CertifiedValue, CertifiedValue, CertifiedValue)
  type DataResult = Option[(CertifiedValues, Seq[BigDecimal])]

  val RegexNumber = """[+-]?\d+(\.\d*)?""".r

  def read(file: File)(implicit ds: DataSet): DataResult = {
    val source = Source.fromFile(file, "UTF-8")
    Datasets.read(source)
  }

  def read(filename: String)(implicit ds: DataSet): DataResult = {
    val source = Source.fromFile(filename, "UTF-8")
    Datasets.read(source)
  }

  private def read(source: Source)(implicit ds: DataSet): DataResult = {
    implicit val linesRegex = ds.LinesRegex
    val fileLines = source.getLines().toVector

    val results = linesBy(ds.CertifiedValuesKeyword, fileLines)
    val data = linesBy(ds.DataLineKeyword, fileLines) map (s => BigDecimal(s.toDouble))

    parseResultLines(results)(ds.ValueSplitter, ds.ExactSymbol) map {
      _ -> data.toVector
    }
  }

  private def parseResultLines(lines: Seq[String])(splitter: Char, exact: String): Option[CertifiedValues] = {
    (for {
      numberCol <- lines map (_.split(splitter)(1))
      number <- RegexNumber findFirstIn numberCol
    } yield (number.toDouble.toBigDecimal(), numberCol contains exact)) match {
      case Seq(mean, stdev, corr) => Some(mean, stdev, corr)
      case _ => None
    }
  }

  private def findAllIntsIn(here: String): Iterator[Int] =
    RegexNumber findAllMatchIn here map (_.matched.toInt)

  private def linesBy(keyword: String, from: Seq[String])(implicit matcher: Regex): Seq[String] = {

    def getDataPosition(s: String): LinesInFile = {
      val lines = for {
        m <- matcher findAllMatchIn s
        line <- findAllIntsIn(m.matched)
      } yield line

      lines.next -> lines.next
    }

    val header = (from dropWhile ( ! _.contains(keyword))) head
    val (firstLine, lastLine) = getDataPosition(header)

    from drop (firstLine - 1) take (lastLine - firstLine + 1)
  }

}
