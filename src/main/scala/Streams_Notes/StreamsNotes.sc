package Streams_Notes

import scala.math.BigInt

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
}