import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import ProcessData._

val x = DenseVector.ones[Double](1000)
val y = (DenseVector.ones[Double](1000))*:*5.0
sqrt((x-y).t*(x-y))