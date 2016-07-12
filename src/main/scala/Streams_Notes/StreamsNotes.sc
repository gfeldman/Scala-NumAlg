package Streams_Notes

import scala.math.BigInt
object StreamsNotes {
  def fibonnaci(n:Int): List[BigInt] = {
  		lazy val fibs: Stream[BigInt] = BigInt(0) #::
  																		BigInt(1) #::
  																		fibs.zip(fibs.tail).map( p => p._1 + p._2)
			fibs.drop(1).take(n).toList
	}                                         //> fibonnaci: (n: Int)List[scala.math.BigInt]
		

	fibonnaci(6)                              //> res0: List[scala.math.BigInt] = List(1, 1, 2, 3, 5, 8)
  

def fizzbuzz(stop: Int) ={
	lazy val fizz: Stream[String] = "" #:: "" #:: "Fizz" #:: fizz
	lazy val buzz: Stream[String] = "" #:: "" #:: "" #:: "" #:: "Buzz" #:: buzz
	fizz.zip(buzz).zip((1 to stop)).filter( { case ((f,b),n) => f=="Fizz" || b=="Buzz"}).map({case ((f,b),n) => f+b+"\t"+n.toString()}).toList
}                                                 //> fizzbuzz: (stop: Int)List[String]
fizzbuzz(100).foreach{println}                    //> Fizz	3
                                                  //| Buzz	5
                                                  //| Fizz	6
                                                  //| Fizz	9
                                                  //| Buzz	10
                                                  //| Fizz	12
                                                  //| FizzBuzz	15
                                                  //| Fizz	18
                                                  //| Buzz	20
                                                  //| Fizz	21
                                                  //| Fizz	24
                                                  //| Buzz	25
                                                  //| Fizz	27
                                                  //| FizzBuzz	30
                                                  //| Fizz	33
                                                  //| Buzz	35
                                                  //| Fizz	36
                                                  //| Fizz	39
                                                  //| Buzz	40
                                                  //| Fizz	42
                                                  //| FizzBuzz	45
                                                  //| Fizz	48
                                                  //| Buzz	50
                                                  //| Fizz	51
                                                  //| Fizz	54
                                                  //| Buzz	55
                                                  //| Fizz	57
                                                  //| FizzBuzz	60
                                                  //| Fizz	63
                                                  //| Buzz	65
                                                  //| Fizz	66
                                                  //| Fizz	69
                                                  //| Buzz	70
                                                  //| Fizz	72
                                                  //| FizzBuzz	75
                                                  //| Fizz	78
                                                  //| Buzz	80
                                                  //| Fizz	81
                                                  //| Fizz	84
                                                  //| Buzz	85
                                                  //| Fizz	87
                                                  //| FizzBuzz	90
                                                  //| Fizz	93
                                                  //| Buzz	95
                                                  //| Fizz	96
                                                  //| Fizz	99
                                                  //| Buzz	100
}