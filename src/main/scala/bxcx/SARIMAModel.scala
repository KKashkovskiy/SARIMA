package bxcx

import breeze.linalg.{DenseMatrix, DenseVector, pinv}

class SARIMAModel(data: Vector[Double], p: Int, d: Int, q: Int) {

    def finiteDiffs(vector: Vector[Double], order: Int): Vector[Double] = {
        if (order < 1) vector
        else {
            if (vector.size < order + 1) throw new RuntimeException()
            else finiteDiffs(vector.indices.dropRight(1)
                .map(idx => vector(idx + 1) - vector(idx)).toVector, order - 1)
        }
    }

    def avg(vector: Vector[Double]) = {
        vector.sum/vector.size
    }

    def vectorMultiplication(vector1: Vector[Double], vector2: Vector[Double]) =
        vector1 zip vector2 map { case (v1, v2) => v1 * v2 } sum

    def solve(a: DenseMatrix[Double], b: DenseVector[Double]): DenseVector[Double] =
        pinv(a) * b

    val diffs: Vector[Double] = finiteDiffs(data, d)
    val boxcoxed: Vector[Double] = BoxCox.transform(data.toList).toVector

    val coefVectors = data.indices.drop(Seq(p, q).max).dropRight(d).map {
        idx => DenseVector(((1.0 +: (1 to p).map(i => diffs(idx - i))) ++ (1 to q).map(j => boxcoxed(idx - j))).toArray)
    }
    val resultV = data.indices.drop(Seq(p, q).max).dropRight(d).map { idx =>
        diffs(idx) - boxcoxed(idx)
    }
    val coefMatrix: DenseMatrix[Double] = DenseMatrix(coefVectors: _ *)
    val resultVector: DenseVector[Double] = DenseVector(resultV: _ *)

    val abcCoenfs: DenseVector[Double] = solve(coefMatrix, resultVector)

    def predict(n: Int) = {
        ???
    }


}
