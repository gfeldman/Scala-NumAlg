package funcClustering

import org.apache.spark.mllib.random.RandomRDDs.uniformVectorRDD
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import breeze.linalg.{DenseVector => BV}


object ClustData {
  
  def uniformData(n: Int, d: Int, sc: SparkContext): RDD[BV[Double]] = {
    uniformVectorRDD(sc, n, d).map(x => BV(x.toArray))
  }
  
  
}