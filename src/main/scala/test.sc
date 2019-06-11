import breeze.linalg.{DenseMatrix, DenseVector, pinv}

import scala.annotation.tailrec

@tailrec
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


val ls = Vector[Double](1, 3, 3, 4, 5)

println(finiteDiffs(ls, 1))
println(finiteDiffs(ls, 2))
println(finiteDiffs(ls, 3))
println(finiteDiffs(ls, 4))

