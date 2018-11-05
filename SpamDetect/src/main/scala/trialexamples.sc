import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import ProcessData._

DenseVector(1, 1)
DenseVector(1, 1) dot DenseVector(2, 3)
cosineSimilarity(DenseVector(1, 2), DenseVector(2, 4))

val x = DenseMatrix.rand[Double](4,3)
DenseMatrix.vertcat(DenseVector(1, 2, 3).toDenseMatrix, x(::, *).map(x => 3 * 4).inner.toDenseMatrix)

argmax(x(::, 2))