import breeze.linalg.{DenseMatrix, DenseVector, pinv}
import bxcx.BoxCox

object Sarima extends App {




    val ls = Vector[Double](1, 3, 3, 4, 5)

    println(finiteDiffs(ls, 1))
    println(finiteDiffs(ls, 2))
    println(finiteDiffs(ls, 3))
    println(finiteDiffs(ls, 4))

    val a = BoxCox.lambdaSearch(ls.toList)
    println(a)
    val b = BoxCox.transform(ls.toList)
    println(b)
}
