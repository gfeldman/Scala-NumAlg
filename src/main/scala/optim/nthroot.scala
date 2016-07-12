package optim

import math.{abs,pow,signum}
import optim.Newton.{NewtonMethod}
import breeze.linalg.{DenseMatrix,DenseVector,norm}
  /**
   * The following function uses Newton's method to compute the nth root of a number c 
   * by finding the root of the function f(x) = x^n -signum(c)*c  
   *
   * INPUT:
   * (1) n - for the n^th root
   * Parameters:
   * (1) x0root - starting point for Newton's method
   * (2) tolroot - tolerance which determines stopping criterion
   * 
   * Note: This implementation is not particularly user friendly.
   * TODO improve printing 
   */



case class nthroot(n: Int, d: Int) {
  def this(n: Int) = this(n,0)
  
}
object nthroot {
    val x0root = 1.0
    val tolroot = 1e-10
    
    def apply(n: Int): Double =>Either[Double, String]  ={
      def rootNewton(c: Double): Either[Double, String] = {
        if ((n % 2 == 0) && signum(c) == -1.0) {
          Right("Error: Solution must be real.")
        } else {
        
          // f(x) = x^n - signum(c)*c 
          def f(x: Double):Double = pow(x, n) - signum(c)*c
          // derivative of f(x) 
          def df(x: Double): Double= n * pow(x, n - 1)
          // solution of f(x) = 0 using Newton's method
          val res = signum(c) * NewtonMethod(x0root,f, df, tolroot)
        
          Left(res)
        }

      }
    
     rootNewton
    }

    def strRt(res: Either[Double,String]):String = res match {
      case Left(v) => v.toString
      case Right(msg) => msg
    }
}









