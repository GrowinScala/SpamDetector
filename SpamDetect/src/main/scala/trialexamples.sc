import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import ProcessData._

val x = DenseVector(1, 2, 3, 4, 5)
val y = Array(1, 2, 3, 4, 5)
val h =
 DenseVector((for (i <- 0 until x.length) yield x.data(i)).toArray)
(0 until x.length).map(i=> if (i<2) x.data(i)
else 0)