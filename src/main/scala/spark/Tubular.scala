package spark

import org.apache.spark.sql.functions._
import org.apache.spark.sql.{DataFrame, SQLContext}
import org.apache.spark.{SparkConf, SparkContext}

object Tubular {

  def withGoodVibes()(df: DataFrame): DataFrame = {
    df.withColumn(
      "chi",
      lit("happy")
    )
  }

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setAppName("HelloWorld Spark App")
    val sc: SparkContext = new SparkContext(conf)
    //val sqlContext: SQLContext = new SQLContext(sc)
    import sqlContext.implicits._

    val df = {
      List("sue", "fan").toDF("name")
    }
    val betterDF = df.transform(Tubular.withGoodVibes())
    betterDF.show()
  }

}
