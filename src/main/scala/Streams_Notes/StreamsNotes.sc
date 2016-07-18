package Streams_Notes

import scala.math.{BigInt,pow}
import breeze.linalg._
import breeze.plot._

object StreamsNotes {
  def fibonacci(n:Int): List[BigInt] = {
  		lazy val fibs: Stream[BigInt] = BigInt(1) #:: BigInt(1) #::
  															 			fibs.zip(fibs.tail).map( p => p._1 + p._2)
			fibs.take(n).toList
	}                                         //> fibonacci: (n: Int)List[scala.math.BigInt]
		

	fibonacci(8)                              //> res0: List[scala.math.BigInt] = List(1, 1, 2, 3, 5, 8, 13, 21)
	
	def goldenRatio(n: Int): Double ={
		val last2fib = fibonacci(n).takeRight(2)
		last2fib(1).toDouble/last2fib(0).toDouble
	}                                         //> goldenRatio: (n: Int)Double
  
  goldenRatio(50)                                 //> res1: Double = 1.618033988749895

def fizzbuzz(stop: Int): List[String] ={
	lazy val fizz: Stream[String] = "" #:: ""  #:: "Fizz" #:: fizz
	lazy val buzz: Stream[String] = "" #:: "" #:: "" #:: "" #:: "Buzz" #:: buzz
	fizz.zip(buzz).zip(1 to stop)
								.filter( { case ((f,b),n) => f=="Fizz" || b=="Buzz"})
								.map({case ((f,b),n) => n.toString()+"\t" + f+b}).toList
}                                                 //> fizzbuzz: (stop: Int)List[String]
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
}                                                 //> lcgNext: (a: scala.math.BigInt, c: scala.math.BigInt, m: scala.math.BigInt)s
                                                  //| cala.math.BigInt => scala.math.BigInt

val a = BigInt(1103515245)                        //> a  : scala.math.BigInt = 1103515245
val c = BigInt(12345)                             //> c  : scala.math.BigInt = 12345
val m = BigInt(pow(2,32).toInt)                   //> m  : scala.math.BigInt = 2147483647
val lcg = lcgNext(a,c,m)                          //> lcg  : scala.math.BigInt => scala.math.BigInt = <function1>
val seed = BigInt(1)                              //> seed  : scala.math.BigInt = 1

val states = Stream.iterate(seed)(lcg(_))         //> states  : scala.collection.immutable.Stream[scala.math.BigInt] = Stream(1, 
                                                  //| ?)
val us = states.map(x => x.doubleValue()/m.doubleValue()).take(1000).toList
                                                  //> us  : List[Double] = List(4.656612875245797E-10, 0.5138700783782965, 0.4398
                                                  //| 008065483537, 0.7894098422440746, 0.46938708772388615, 0.1094664871271078, 
                                                  //| 0.36136545304272577, 0.44898528067813503, 0.008931668013768115, 0.816477734
                                                  //| 510078, 0.23493939602511907, 0.16481704319120247, 0.7973211248392803, 0.420
                                                  //| 6997614450286, 0.32245803406576534, 0.4643071389125228, 0.15230734048053032
                                                  //| , 0.14567659057009807, 0.5337322035495807, 0.364411212207941, 0.12039872078
                                                  //| 243584, 0.8619220204939703, 0.6163043634110617, 0.5841324644089362, 0.57468
                                                  //| 67542968536, 0.46615290570359347, 0.9449685909529071, 0.1627077363257798, 0
                                                  //| .5149440520978272, 0.812032317655176, 0.9651751974435407, 0.474838010256568
                                                  //| 9, 0.22359589777122993, 0.9100194875663237, 0.7765319136793408, 0.974182378
                                                  //| 4886777, 0.07262168362393122, 0.9965807003884486, 0.7514362436493096, 0.512
                                                  //| 5533419254019, 0.6903843598861175, 0.04390281534004156, 0.02616147185962715
                                                  //| 6, 0.028742816778245764
                                                  //| Output exceeds cutoff limit.

//val f = Figure()
//val p = f.subplot(0)
//p += hist(us,10)

}