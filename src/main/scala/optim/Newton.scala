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
  
  def NewtonMethod(x0: DenseVector[Double])(f: DenseVector[Double] => Double, df: DenseVector[Double] => DenseVector[Double], tol: Double): DenseVector[Double] = {
    def improve(guess: DenseVector[Double]): DenseVector[Double] = guess - f(guess)*(1.0/df(guess))
    def isGoodEnough(guess: DenseVector[Double]): Boolean = norm(f(guess)*(1.0/df(guess)),1) < tol
    lazy val guesses: Stream[DenseVector[Double]] = x0 #:: (guesses.map(improve))
    guesses.filter(isGoodEnough(_)).take(3).toList.last
  }

  def NewtonMethod(x0: Double,f: Double => Double, df: Double => Double, tol: Double): Double = {
    val x0m = DenseVector(x0)
    def fm(x: DenseVector[Double]): Double= f(x(0))
    def dfm(x: DenseVector[Double]): DenseVector[Double] = DenseVector(df(x(0)))
    val res = NewtonMethod(x0m)(fm,dfm,tol)
    res(0) 
  }
 


}
