[![Build Status](https://travis-ci.org/jvican/statla.svg?branch=master)](https://travis-ci.org/jvican/statla)

Statla, stats in Scala
======================

__Statla__ is an easy-to-use library for parallel computation of statistics descriptors like _mean_,
_variance_, _standard deviation_, _skewness_, _kurtosis_, _correlation_ and _pearson coefficient_ (at this moment). Its goal is to compute
__descriptive__ and __correlative__ statistics of any numeric collection in a fast and reliable way.

Statla computes statistics incrementally, avoiding redundancy in computations while adding new elements to a collection.
It is built on top of __Spire__ to use rich type classes and missing implementation of well-known numeric classes like BigDecimal. As
well as Spire, it uses specialization.

In order to strike a balance between __fast__ and __precise__ computations, users can choose the types of the result and the collection
from which statistics are derived. Therefore, if you want precision over speed you can use BigDecimal instead of Double. The only limitation is
that types must have an implicit __Fractional[T]__.

To prove its correctness, Statla uses __NIST StRD__ datasets, ensuring the numerically-stable property of the algorithms used.

This library is based on the following papers:

1.   [Computing Higher-Order Moments Online by Timothy B. Terriberry](http://people.xiph.org/~tterribe/notes/homs.html)
2.   [Formulas for Robust, One-Pass Parallel Computation of Covariances and Arbitrary-Order Statistical Moments by Philippe Pebay [Peb08]](http://prod.sandia.gov/techlib/access-control.cgi/2008/086212.pdf)
3.   [Note on a Method for Calculating Corrected Sums of Squares and Products by B. P. Welford [Wel62]](http://zach.in.tu-clausthal.de/teaching/info_literatur/Welford.pdf)

_[to be added...]_

Example
=======

```scala
import statla.Util._
import scala.util.Random

val v = Vector.fill[Int](100)(Math.abs(Random.nextInt()))

// Computing descriptive statistics
println(Util.compute[Double, Int](v).stats) // Fast
println(Util.compute[BigDecimal, Int](v).stats) // High precision
```

The future is uncertain, but...
===============================

Currently, this library is under development. Nevertheless, my goal is to implement incremental quantile estimators
and more cool stuff. If you want to contribute, please contact me here or send me an email to _jorgevc @ fastmail dot es_.
