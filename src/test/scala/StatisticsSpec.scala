import org.scalatest.FlatSpec

class StatisticsSpec extends FlatSpec {
  import RandomElems._

  def withInts(test: Elems[Int] => Any) = {
    test(ints)
  }

  def withDoubles(test: Elems[Double] => Any) = {
    test(doubles)
  }

  "Mean" should "be equal to incremental mean" in withInts { e =>
    val mean = Statistics.mean(e.elems)

    assertResult(Statistics.mean(e.updatedElems)) {
      Statistics.updateMean(mean, e.elems.length, e.elemToUpdate)
    }
  }

  "Standard deviation" should "be 0 if there aren't 2 elements or more2 for any numeric type" in {
    assert(Statistics.stdev[Int](Seq.empty[Int]) == 0.0)
    assert(Statistics.stdev[Double](Seq.empty[Double]) == 0.0)
    assert(Statistics.stdev[Int](Seq(1)) == 0.0)
    assert(Statistics.stdev[Double](Seq(1.0)) == 0.0)
  }

  "Variance" should "be 0 if there aren't 2 elements or more for any numeric type" in {
    assert(Statistics.variance[Int](Seq.empty[Int]) == 0.0)
    assert(Statistics.variance[Double](Seq.empty[Double]) == 0.0)
    assert(Statistics.variance[Int](Seq(1)) == 0.0)
    assert(Statistics.variance[Double](Seq(1.0)) == 0.0)
  }

  it should "be the square root of the standard deviation" in withInts { e =>
    assertResult(Statistics.stdev(e.elems)) {
      Math.sqrt(Statistics.variance(e.elems))
    }
  }

  it should "have the same result than the incremental variance implementation" in withInts { e =>
    val lastVariance = Statistics.variance(e.elems)
    val lastMean = Statistics.mean(e.elems)
    val lastN = e.elems.length

    assertResult(Statistics.variance(e.updatedElems)) {
      Statistics.updateVariance(lastVariance, lastMean, lastN, e.elemToUpdate)
    }
  }
}
