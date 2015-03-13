package statla

trait PrintableStatistics {
  val delimiter: Char = '='
  val stats: String

  def delimiterOf(s: String): String = delimiter.toString * s.length
  def delimiterOf(size: Int): String = delimiter.toString * size
}
