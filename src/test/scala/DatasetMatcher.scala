import org.scalatest.matchers.{MatchResult, Matcher}

trait DatasetMatcher {
  import Datasets.CertifiedValue
  import statla.Utils.roundToLowerScale

  class ResultMatchWithCertifiedResult(cert: CertifiedValue) extends Matcher[BigDecimal] {
    override def apply(res: BigDecimal): MatchResult = cert match {
      case (correct, true) =>
        MatchResult(
          res == correct,
          s"""Result $res did not match exactly with the certified value $correct""",
          s"""Result $res matches exactly with the certified value $correct"""
        )

      case (correct, false) =>
        val rounded = roundToLowerScale(correct, res)

        MatchResult(
          rounded == correct,
          s"""Result $res did not match with the certified value $correct""",
          s"""Result $res matches with the certified value $correct"""
        )

      case _ => 
        MatchResult(matches = false, "", "Error while pattern-matching in ResultMatchWithCertifiedResult matcher, unexpected type")
    }
  }

  def matchWithCertifiedValue(cert: CertifiedValue) = new ResultMatchWithCertifiedResult(cert)
}

object DatasetMatcher extends DatasetMatcher
