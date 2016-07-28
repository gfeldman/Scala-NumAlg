package funcClustering

import java.io.File;
import java.io.FileWriter;

import org.apache.spark.rdd.RDD
import breeze.linalg.{DenseVector => BV,DenseMatrix}

object ResultsWriter {
  
  type State = Array[BV[Double]]
  type Point = BV[Double]
  type Labels = Array[Int]
  
  def writeResults(data: RDD[BV[Double]], iterations: List[State], iter_labels: List[Labels] ) = {
    
    
    // write data out 
    val fwData = new FileWriter(new File("results/data.csv"))
    fwData.write("x,y\n")
    data.map( x => x.toArray.mkString(",") + '\n')
        .collect()
        .foreach{ fwData.write }

    fwData.close()
    
    
    // write labels out for each iteration 
    val fwLabels = new FileWriter(new File("results/labels.csv"))
    val num_iter = iterations.length
    
    val labelHeader = (1 to num_iter).map( "l" + _.toString)
                                     .reduce(_ + "," + _) + "\n"                                  
    fwLabels.write(labelHeader)
    
    val num_points = data.count().toInt
    
    val lMatrix = DenseMatrix.zeros[Int](num_points,num_iter)
    for( i <- 0 until num_iter) {
      lMatrix(::,i) := BV(iter_labels(i))   
    }
    for( i <- 0 until num_points) {
      fwLabels.write(lMatrix(i,::).t.toArray.mkString(",") + "\n")
    }
    
    fwLabels.close()
    
                                 
  }
}