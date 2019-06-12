package bxcx

import breeze.linalg.{DenseMatrix, DenseVector, pinv}

case class Season(period: Int, p: Int, d: Int, q: Int)

class SARIMAModel(data: Vector[Double], p: Int, d: Int, q: Int, seasons: Seq[Season] = Nil) {

    val diffs: Vector[Double] = finiteDiffs(data, d)
    val boxcoxed: Vector[Double] = BoxCox.transform(data.toList).toVector

    def finiteDiffs(vector: Vector[Double], order: Int, lag: Int = 0): Vector[Double] = {
        if (order < 1) vector
        else {
            if (vector.size < order + 1 + lag) throw new RuntimeException()
            else finiteDiffs(vector.indices.dropRight(1 + lag)
                .map(idx => vector(idx + 1 + lag) - vector(idx)).toVector, order - 1, lag)
        }
    }

    def avg(vector: Vector[Double]) = {
        vector.sum/vector.size
    }

    def vectorMultiplication(vector1: Vector[Double], vector2: Vector[Double]) =
        vector1 zip vector2 map { case (v1, v2) => v1 * v2 } sum

    def solve(a: DenseMatrix[Double], b: DenseVector[Double]): DenseVector[Double] =
        pinv(a) * b

    def createVectors(dropFirst: Int, dropLast: Int) = {
        data.indices.drop(dropFirst).dropRight(dropLast).map { idx =>
            var v = (1.0 +: (1 to p).map(i => diffs(idx - i))) ++ (1 to q).map(j => boxcoxed(idx - j))
            seasons.foreach { season =>
                val sDiffs = finiteDiffs(data, season.d, season.period)
                if (idx >= Seq(season.p, season.q, season.period).max) {
                    v = v ++ (season.period until season.period + season.p).map(i => sDiffs(idx - i))
                    v = v ++ (season.period until season.period + season.q).map(i => boxcoxed(idx - i - season.period))
                } else v = v :+ 0 :+ 0
            }
            DenseVector(v: _ *)
        }
    }

    val coefVectors = createVectors(Seq(p, q).max, d)

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
