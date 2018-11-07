import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import ProcessData._

val x = DenseVector(1, 2, 3, 4, 5)
val matrix = DenseMatrix((1.0,0.0,1.2),(2.1,2.2,2.4))
val y = Array(1, 2, 3, 4, 5)
val h =
 DenseVector((for (i <- 0 until x.length) yield x.data(i)).toArray)
(0 until x.length).map(i=> if (i<2) x.data(i)
else 0)

matrix(*, ::).map(row => argmin(row))