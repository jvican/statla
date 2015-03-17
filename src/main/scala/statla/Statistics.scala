package statla

object Statistics {

  // TODO Implement new Autocorrelation in an incremental way

/*  def computeAutocorrelation[T: Numeric](elems: Seq[T]): Autocorrelation =
    elems.init.zip(elems.tail).foldLeft(new Autocorrelation(emptySample, 0.0, 0.0))(_ + _)

  def autocorrelateByOne[T: Numeric](elems: Seq[T]): BigDecimal =
    computeAutocorrelation(elems).coefficient*/
}
