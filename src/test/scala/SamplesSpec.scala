import org.scalatest.{FlatSpec, Matchers}

class SamplesSpec extends FlatSpec with Matchers {
  import RandomElems._

  def withSampleInts(test: SampleTest[Int] => Any): Unit = {
    test(sampleInts)
  }
  
  def withSampleDoubles(test: SampleTest[Double] => Any): Unit = {
    test(sampleDouble)
  }

  behavior of "A Sample"

  it should "have the same mean than the standard implementation of mean with Ints" in withSampleInts { t =>
    val (e, s, marginError) = t
    Statistics.mean(e.elems) should be (s.mean +- marginError)
  }

  it should "have the same stdev than the standard implementation of stdev with Ints" in withSampleInts { t =>
    val (e, s, marginError) = t
    Statistics.stdev(e.elems) should be (s.stdev +- marginError)
  }

  it should "have the same variance than the standard implementation of variance with Ints" in withSampleInts { t =>
    val (e, s, marginError) = t
    Statistics.variance(e.elems) should be (s.variance +- marginError)
  }

  it should "have the same mean than the standard implementation of mean with Doubles" in withSampleDoubles { t =>
    val (e, s, marginError) = t
    Statistics.mean(e.elems) should be (s.mean +- marginError)
  }

  it should "have the same stdev than the standard implementation of stdev with Doubles" in withSampleDoubles { t =>
    val (e, s, marginError) = t
    Statistics.stdev(e.elems) should be (s.stdev +- marginError)
  }

  it should "have the same variance than the standard implementation of variance with Doubles" in withSampleDoubles { t =>
    val (e, s, marginError) = t
    Statistics.variance(e.elems) should be (s.variance +- marginError)
  }
}
