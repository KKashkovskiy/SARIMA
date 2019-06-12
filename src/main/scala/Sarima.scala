import bxcx.{SARIMAModel, Season}

import scala.io.Source

object Sarima extends App {
    def avg(vector: Vector[Double]) = {
        vector.sum/vector.size
    }

    val data = Source.fromFile("/home/kkashkovskij/IdeaProjects/SARIMA/src/main/resources/mow_spb_ts_fill_zeros.csv").getLines.drop(1).map { line =>
        line.split(",")(1).toDouble
    }.toVector
    val (data1, data2) = data.splitAt(730)

    val model = new SARIMAModel(data1, 1, 1, 1, Seq(Season(7, 1, 1, 1)))
    val result = model.predict(data2.size)

    val diff = data2.zip(result.drop(730)).map{ case (b, p) => b - p}

    println(avg(diff))

}
