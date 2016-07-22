package funcClustering

import org.apache.spark.SparkContext
import org.apache.spark.SparkConf
import org.apache.spark.mllib.random.RandomRDDs.uniformVectorRDD

import breeze.linalg.{DenseVector => BV,norm}

import KMeans._

import org.apache.log4j.Logger
import org.apache.log4j.Level


object Main extends App{
  
  Logger.getLogger("org").setLevel(Level.OFF)
  Logger.getLogger("akka").setLevel(Level.OFF)
  
  
  val conf = new SparkConf().setAppName("K-Means Convergence")
                            .setMaster("local")
  val sc = new SparkContext(conf)

  // Generate some example data
  val data = uniformVectorRDD(sc, 500, 2).map(x => BV(x.toArray))
  
  /**
   * Parameters for K-Means
   * (1) k - the number of clusters
   * (2) tol - stopping criterion  norm difference between successive k-means guesses is less than tol. 
   */
  
  val k = 4
  val tol = 1e-6
  
  val kms  = kmeanses(k,data,tol)
  kms.map(_.mkString("[",",","]")).foreach{println}  
}