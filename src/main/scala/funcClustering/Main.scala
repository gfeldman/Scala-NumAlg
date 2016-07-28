package funcClustering

import org.apache.spark.{SparkConf, SparkContext}
import breeze.linalg.{DenseVector => BV,norm}


import org.apache.log4j.Logger
import org.apache.log4j.Level

import ClustData._
import KMeans._
import ResultsWriter._

object Main extends App {
  
  // Set up Spark environment
  
  Logger.getLogger("org").setLevel(Level.OFF)
  Logger.getLogger("akka").setLevel(Level.OFF)
  
  val conf = new SparkConf().setAppName("K-Means Convergence")
                            .setMaster("local")
  val sc = new SparkContext(conf)

  
  // generate some sample data 
  // returns an RDD of dense breeze vectors 
  val data = uniformData(500,2,sc)
  
  
  // running K-Means 
  
  /**
   * Parameters for K-Means
   * (1) k - the number of clusters
   * (2) tol - stopping criterion  norm difference between successive k-means guesses is less than tol. 
   */
  
  val k = 5
  val tol = 1e-6
  
  val (kms,labels)  = kmeanses(k,data,tol)
  
  writeResults(data,kms,labels)
 
}