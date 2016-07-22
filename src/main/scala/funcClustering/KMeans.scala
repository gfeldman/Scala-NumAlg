package funcClustering
import org.apache.spark.rdd.RDD
import org.apache.spark.mllib.linalg.DenseVector
import breeze.linalg.{DenseVector => BV,norm}

object KMeans {
  
  type State = Array[BV[Double]]
  type VecCollector = (BV[Double], Int)
  
  def newMeansFromData(data: RDD[BV[Double]])(oldMeans: State): State ={
   
   /**
    * Find nearest mean index for each data point 
    */
    val indexedOldMeans = oldMeans.zipWithIndex
    
    def nearestMeanIndex(point: BV[Double]):Int = { 
                indexedOldMeans
                .map{case (m,ind) => (norm(m-point),ind)}
                .minBy(_._1)
                ._2
             
    }
   
    /**
     * Compute new cluster means 
     */
   
   val createCountVecCombiner = (v: BV[Double]) => (v, 1)
   val countVecCombiner = (collector: VecCollector, v: BV[Double]) => {
         val (sumVecs,numVecs) = collector
        (sumVecs + v,numVecs + 1)
   }
   val countVecMerger = (collector1: VecCollector, collector2: VecCollector) => {
        val (sumVecs, numVecs) = collector1
        val (newVec, numNewVec) = collector2
        (sumVecs + newVec,numVecs + numNewVec)
   }
  
 
  val newMeans = data.map(point => (nearestMeanIndex(point),point))
                       .combineByKey( createCountVecCombiner, countVecCombiner,countVecMerger)
                       .sortBy(_._1)
                       .map{ case (index,(v,count)) => v/(count.toDouble)}
                       .collect()
                       
   newMeans
  }
  
  def kmeanses(k: Int, data: RDD[BV[Double]], tol: Double): List[State] = {
    
    val initialKMeans = data.takeSample(false, k)
    def nextKMeans = newMeansFromData(data)(_)
    
    val guesses = Stream.iterate(initialKMeans)(nextKMeans)
    
    
    val pairConvInd = guesses.zip(guesses.tail)
                             .indexWhere{ case (oldMeans , newMeans ) => converged(oldMeans,newMeans,tol)} 
    
    // We add two to the converged index because the indices start at 0  
    // and we want one additional mean after convergence
    guesses.take(pairConvInd+2).toList
  }
      
  
  def converged(oldMeans: State, newMeans: State, tol: Double): Boolean = {
    oldMeans.zip(newMeans)
            .map{ case (oldMean,newMean) => norm(newMean-oldMean,2)}
            .forall( _ < tol)
  }
  
  
  

}