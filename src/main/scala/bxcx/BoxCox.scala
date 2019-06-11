package bxcx

import java.util

import _root_.util.Stats
import org.apache.commons.math3.optimization.GoalType
import org.apache.commons.math3.optimization.univariate.BrentOptimizer

import scala.collection.JavaConverters._

/** Box Cox Transformation
  *
  * @author navdeepgill
  */
object BoxCox {
    /**
      * Find the optimal lambda for a given time series data set and conduct transformation
      *
      * @param  data a List<Double> of time series data
      * @return Time series List<Double> with optimal Box Cox lambda transformation
      */
    def transform(data: List[Double]): List[Double] = transform(data, lambdaSearch(data))

    /**
      * Calculate a Box Cox Transformation for a given lambda
      *
      * @param  data a List<Double> of time series data
      * @param lam   desired lambda for transformation
      * @return Time series List<Double> with desired Box Cox transformation
      */
    def transform(data: List[Double], lam: Double): List[Double] = {
        val transform = new util.ArrayList[Double]
        if (lam == 0) {
            var i = 0
            while ( {
                i < data.size
            }) {
                transform.add(Math.log(data(i)))
                i += 1
            }
        }
        else {
            var i = 0
            while ( {
                i < data.size
            }) {
                transform.add((Math.pow(data(i), lam) - 1.0) / lam)
                i += 1
            }
        }
        transform.asScala.toList
    }

    /**
      * Find the optimal lambda for a given time series data set with default lower/upper bounds for lambda search
      *
      * @param  data a List<Double> of time series data
      * @return Time series List<Double> with optimal Box Cox lambda transformation
      */
    def lambdaSearch(data: List[Double]): Double = lambdaSearch(data, -1, 2)

    /**
      * Find the optimal lambda for a given time series data set given lower/upper bounds for lambda search
      *
      * @param  data a List<Double> of time series data
      * @param lower lower bound for lambda search
      * @param upper upper bound for lambda search
      * @return Time series List<Double> with optimal Box Cox lambda transformation
      */
    def lambdaSearch(data: List[Double], lower: Double, upper: Double): Double = {
        val solver = new BrentOptimizer(1e-10, 1e-14)
        val lambda = solver.optimize(100, (x: Double) => lambdaCV(data, x), GoalType.MINIMIZE, lower, upper).getPoint
        lambda
    }

    /**
      * Compute the coefficient of variation
      *
      * @param data a List<Double> of time series data
      * @param lam  lambda
      * @return Coefficient of Variation
      */
    private def lambdaCV(data: List[Double], lam: Double) = {
        val iter = data.iterator
        val avg = new util.ArrayList[Double]
        val result = new util.ArrayList[Double]
        while ( {
            iter.hasNext
        }) {
            val l = new util.ArrayList[Double]
            l.add(iter.next)
            if (iter.hasNext) l.add(iter.next)
            avg.add(Stats.average(l.asScala.toList.map(Double.box).asJava))
            result.add(Stats.standardDeviation(l.asScala.toList.map(Double.box).asJava))
        }
        var i = 0
        while ( {
            i < result.size
        }) {
            result.set(i, result.get(i) / Math.pow(avg.get(i), 1 - lam))

            i += 1
        }
        Stats.standardDeviation(result.asScala.toList.map(Double.box).asJava) / Stats.average(result.asScala.toList.map(Double.box).asJava)
    }
}
