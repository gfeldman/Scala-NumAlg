package optim
import math.{abs,pow,signum,sqrt}
import breeze.linalg.{DenseMatrix,DenseVector,norm}

object Newton {

  /**
   *
   *  This class implements Newton's method for finding roots of a function f(x)
   *  (i.e. f(x) = 0)
   *  
   *	The primary method is NewtonMethod which takes the following parameters
   * 	(1) f - real function of one variable 
   * 	(2) df - derivative of f
   * 	(3) x0 - a starting point
   * 	(4) tolerance which determines the stopping criterion
   * 
   * 	The helper method isGoodEnough determines if the stopping criterion is met.
   */
  
  def NRMethod(x0: DenseVector[Double],f: DenseVector[Double] => Double, df: DenseVector[Double] => DenseVector[Double], tol: Double): DenseVector[Double] = {
    
    def improve(guess: DenseVector[Double]): DenseVector[Double] = guess - f(guess)*(1.0/df(guess))
    val guesses = Stream.iterate(x0)(improve)
    
    val convergedGuesses = guesses.zip(guesses.tail)
                                  .map{ case (oldGuess ,newGuess) => (newGuess,norm(newGuess-oldGuess,1))}
                                  .filter{ case (newGuess,normDiff) => normDiff < tol}
                                  .map{ case (newGuess, normDiff) => newGuess}
                                  
    convergedGuesses.take(1).toList.last
    
  }

  def NewtonMethod1D(x0: Double,f: Double => Double, df: Double => Double, tol: Double): Double = {
    /**
     * Helper method for 1 dimension that wraps the values in vectors. 
     */
    val x0m = DenseVector(x0)
    def fm(x: DenseVector[Double]): Double= f(x(0))
    def dfm(x: DenseVector[Double]): DenseVector[Double] = DenseVector(df(x(0)))
    val res = NRMethod(x0m,fm,dfm,tol)
    res(0) 
  }
 


}

