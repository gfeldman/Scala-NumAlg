package Streams_Notes

import scala.math.{BigInt,pow}
import breeze.linalg._
import breeze.plot._

object StreamsNotes {
  def fibonacci(n:Int): List[BigInt] = {
  		lazy val fibs: Stream[BigInt] = BigInt(1) #:: BigInt(1) #::
  															 			fibs.zip(fibs.tail).map( p => p._1 + p._2)
			fibs.take(n).toList
	}                                         //> fibonacci: (n#11598697: Int#1169)List#3097[scala#29.math#2816.BigInt#36010]
		

	fibonacci(8)                              //> res0: List#3097[scala#29.math#2816.BigInt#36010] = List(1, 1, 2, 3, 5, 8, 13
                                                  //| , 21)
	
	
	val x1 = DenseVector.fill(2)(2.0)         //> x1  : breeze#43.linalg#42725.DenseVector#47689[Double#1685] = DenseVector(2.
                                                  //| 0, 2.0)
  val x2 = DenseVector.fill(x1.length)(3.0)       //> x2  : breeze#43.linalg#42725.DenseVector#47689[Double#1685] = DenseVector(3.
                                                  //| 0, 3.0)
  val z = Array(x1,x2)                            //> z  : Array#971[breeze#43.linalg#42725.DenseVector#47689[Double#1685]] = Arra
                                                  //| y(DenseVector(2.0, 2.0), DenseVector(3.0, 3.0))
  z.zipWithIndex                                  //> res1: Array#971[(breeze#43.linalg#42725.DenseVector#47689[Double#1685], Int#
                                                  //| 1169)] = Array((DenseVector(2.0, 2.0),0), (DenseVector(3.0, 3.0),1))
	def goldenRatio(n: Int): Double ={
		val last2fib = fibonacci(n).takeRight(2)
		last2fib(1).toDouble/last2fib(0).toDouble
	}                                         //> goldenRatio: (n#11600115: Int#1169)Double#1685
  
  goldenRatio(50)                                 //> res2: Double#1685 = 1.618033988749895

def fizzbuzz(stop: Int): List[String] ={
	lazy val fizz: Stream[String] = "" #:: ""  #:: "Fizz" #:: fizz
	lazy val buzz: Stream[String] = "" #:: "" #:: "" #:: "" #:: "Buzz" #:: buzz
	fizz.zip(buzz).zip(1 to stop)
								.filter( { case ((f,b),n) => f=="Fizz" || b=="Buzz"})
								.map({case ((f,b),n) => n.toString()+"\t" + f+b}).toList
}                                                 //> fizzbuzz: (stop#11600209: Int#1169)List#3097[String#17435]
fizzbuzz(15).foreach{println}                     //> 3	Fizz
                                                  //| 5	Buzz
                                                  //| 6	Fizz
                                                  //| 9	Fizz
                                                  //| 10	Buzz
                                                  //| 12	Fizz
                                                  //| 15	FizzBuzz


/**
* Generating uniforms using lcg
*/
def lcgNext(a: BigInt,c: BigInt, m: BigInt): BigInt => BigInt = {
	x => ((a*x + c).mod(m))
}                                                 //> lcgNext: (a#11597853: scala#29.math#2816.BigInt#36010, c#11597854: scala#29
                                                  //| .math#2816.BigInt#36010, m#11597855: scala#29.math#2816.BigInt#36010)scala#
                                                  //| 29.math#2816.BigInt#36010 => scala#29.math#2816.BigInt#36010

val a = BigInt(1103515245)                        //> a  : scala#29.math#2816.BigInt#36010 = 1103515245
val c = BigInt(12345)                             //> c  : scala#29.math#2816.BigInt#36010 = 12345
val m = BigInt(pow(2,32).toInt)                   //> m  : scala#29.math#2816.BigInt#36010 = 2147483647
val lcg = lcgNext(a,c,m)                          //> lcg  : scala#29.math#2816.BigInt#36010 => scala#29.math#2816.BigInt#36010 =
                                                  //|  <function1>
val seed = BigInt(1)                              //> seed  : scala#29.math#2816.BigInt#36010 = 1

val states = Stream.iterate(seed)(lcg(_))         //> states  : scala#29.collection#2820.immutable#5911.Stream#7932[scala#29.math
                                                  //| #2816.BigInt#36010] = Stream(1, ?)
val us = states.map(x => x.doubleValue()/m.doubleValue()).take(1000).toList
                                                  //> us  : List#3097[Double#1685] = List(4.656612875245797E-10, 0.51387007837829
                                                  //| 65, 0.4398008065483537, 0.7894098422440746, 0.46938708772388615, 0.10946648
                                                  //| 71271078, 0.36136545304272577, 0.44898528067813503, 0.008931668013768115, 0
                                                  //| .816477734510078, 0.23493939602511907, 0.16481704319120247, 0.7973211248392
                                                  //| 803, 0.4206997614450286, 0.32245803406576534, 0.4643071389125228, 0.1523073
                                                  //| 4048053032, 0.14567659057009807, 0.5337322035495807, 0.364411212207941, 0.1
                                                  //| 2039872078243584, 0.8619220204939703, 0.6163043634110617, 0.584132464408936
                                                  //| 2, 0.5746867542968536, 0.46615290570359347, 0.9449685909529071, 0.162707736
                                                  //| 3257798, 0.5149440520978272, 0.812032317655176, 0.9651751974435407, 0.47483
                                                  //| 80102565689, 0.22359589777122993, 0.9100194875663237, 0.7765319136793408, 0
                                                  //| .9741823784886777, 0.07262168362393122, 0.9965807003884486, 0.7514362436493
                                                  //| 096, 0.5125533419254019, 0.6903843598861175, 0.04390281534004156, 0.0261614
                                                  //| 71859627156, 0.02874281
                                                  //| Output exceeds cutoff limit.

//val f = Figure()
//val p = f.subplot(0)
//p += hist(us,10)

}