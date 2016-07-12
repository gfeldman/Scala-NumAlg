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
  
  println(s"square root of -2:\t ${strRt(sqrt(-2))}")
  println(s"square root of 2:\t ${strRt(sqrt(2))}")
  println(s"cube root of 10:\t ${strRt(cubert(10))}")
  println(s"cube root of -10: \t ${strRt(cubert(-10))}")
  println(s"fourth root of 16: \t ${strRt(fourthrt(16))}")
  

}