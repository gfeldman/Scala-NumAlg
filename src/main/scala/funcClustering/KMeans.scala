package funcClustering
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.linalg.{DenseVector,Vectors}
import breeze.linalg.{DenseVector => BV,norm}

object KMeans {
  
  type State = Array[BV[Double]]
  type VecCollector = (BV[Double], Int)
  type Point = BV[Double]
  type Labels = Array[Int]
  
  def labelPoint(point: Point, centroid: State):Int = { 
       
   /**
    * Find nearest mean index for each data point 
    */
  
	  centroid.zipWithIndex
	          .map{case (m,ind) => (norm(m-point),ind)}
	          .minBy(_._1)
	          ._2
             
  }
  
  def newMeans(data: RDD[Point])(oldMeans: State): State ={
    
    /**
     * Compute new cluster means 
     */
   
   val createCountVecCombiner = (v: Point) => (v, 1)
   val countVecCombiner = (collector: VecCollector, v: BV[Double]) => {
         val (sumVecs,numVecs) = collector
        (sumVecs + v,numVecs + 1)
   }
   val countVecMerger = (collector1: VecCollector, collector2: VecCollector) => {
        val (sumVecs, numVecs) = collector1
        val (newVec, numNewVec) = collector2
        (sumVecs + newVec,numVecs + numNewVec)
   }
  
 
  val newMeans = data.map(point => (labelPoint(point,oldMeans),point))
                       .combineByKey( createCountVecCombiner, countVecCombiner,countVecMerger)
                       .sortBy(_._1)
                       .map{ case (index,(v,count)) => v/(count.toDouble)}
                       .collect()
                       
   newMeans
  }
  
  def kmeanses(k: Int, data: RDD[Point], tol: Double): (List[State],List[Labels]) = {
    
    val initialKMeans = data.takeSample(false, k)
    def nextKMeans = newMeans(data)(_)
    
    val guesses = Stream.iterate(initialKMeans)(nextKMeans)
    
    
    val pairConvInd = guesses.zip(guesses.tail)
                             .indexWhere{ case (oldMeans , newMeans ) => converged(oldMeans,newMeans,tol)} 
    
    // We add two to the converged index because the indices start at 0  
    // and we want one additional mean after convergence
     
    val iterations = guesses.take(pairConvInd+2).toList
    val list_of_labels = for (centroids <- iterations ) yield( data.map(labelPoint(_,centroids)).collect() )
    (iterations,list_of_labels)
  }
      
  
  def converged(oldState: State, newState: State, tol: Double): Boolean = {
    oldState.zip(newState)
            .map{ case (oldState,newState) => norm(newState-oldState,2)}
            .forall( _ < tol)
  }
  
  
  

}