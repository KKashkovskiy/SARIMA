package bxcx

import breeze.linalg.{DenseMatrix, DenseVector, pinv}

import scala.annotation.tailrec

case class Season(period: Int, p: Int, d: Int, q: Int)

class SARIMAModel(rawData: Vector[Double], p: Int, d: Int, q: Int, seasons: Seq[Season] = Nil) {

    val data: Vector[Double] = rawData/*BoxCox.transform(rawData.toList).toVector*/
    val diffs: Vector[Double] = finiteDiffs(data, d)
    val stationary: Vector[Double] = finiteDiffs(data, 1)

    def finiteDiffs(vector: Vector[Double], order: Int, lag: Int = 0): Vector[Double] = {
        if (order < 1) vector
        else {
            if (vector.size < order + 1 + lag) throw new RuntimeException()
            else finiteDiffs(vector.indices.dropRight(1 + lag)
                .map(idx => vector(idx + 1 + lag) - vector(idx)).toVector, order - 1, lag)
        }
    }

    def vectorMultiplication(vector1: Vector[Double], vector2: Vector[Double]) =
        vector1 zip vector2 map { case (v1, v2) => v1 * v2 } sum

    def solve(a: DenseMatrix[Double], b: DenseVector[Double]): DenseVector[Double] =
        pinv(a) * b

    def createVector(idx: Int, data: Vector[Double] = data) = {
        val diffs: Vector[Double] = finiteDiffs(data, d)
        var v = (1.0 +: (1 until p).map(i => diffs(idx - i))) ++ (1 until q).map(j => data(idx - j))
        seasons.foreach { season =>
            val sDiffs = finiteDiffs(data, season.d, season.period)
            if (idx > Seq(season.p, season.q, season.period).max) {
                v = v ++ (season.period until season.period + season.p).map(i => sDiffs(idx - i - season.d))
                v = v ++ (season.period until season.period + season.q).map(i => data(idx - i))
            } else v = v :+ 0.0 :+ 0.0
        }
        DenseVector(v: _ *)
    }

    def createVectors(dropFirst: Int, dropLast: Int) = {
        data.indices.drop(dropFirst).dropRight(dropLast).map { idx => createVector(idx) }
    }

    val coefVectors = createVectors(Seq(p, q).max, d)

    val resultV = data.indices.drop(Seq(p, q).max).dropRight(d).map { idx =>
        diffs(idx)
    }

    val coefMatrix: DenseMatrix[Double] = DenseMatrix(coefVectors: _ *)
    val resultVector: DenseVector[Double] = DenseVector(resultV: _ *)

    val abcCoenfs: DenseVector[Double] = solve(coefMatrix, resultVector)

    def predict(n: Int): Vector[Double] = {
        val s = n + data.length
        @tailrec
        def go(data: Vector[Double]): Vector[Double] = {
            if (data.length >= s) data else {
                val newData = createVector(data.length - 1, data) * abcCoenfs
                go(data :+ newData.data.toVector.sum)
            }
        }
        go(data)
    }
}
