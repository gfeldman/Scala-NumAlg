package optim
import optim.nthroot._


/**
 * The purpose of this file is to illustrate the use of the nth root function 
 * based on the Newton-Raphson method. 
 */
object Main extends App {
    
  
  def sqrt = nthroot(2)
  def cubert = nthroot(3)
  def fourthrt = nthroot(4)
  
  println("square root of -2:\t%s".format(strRt(sqrt(-2))))
  println("square root of 2:\t%s".format(strRt(sqrt(2))))
  println("cube root of 10:\t%s".format(strRt(cubert(10))))
  println("cube root of -10:\t%s".format(strRt(cubert(-10))))
  println("fourth root of 16:\t%s".format(strRt(fourthrt(16))))
  

}