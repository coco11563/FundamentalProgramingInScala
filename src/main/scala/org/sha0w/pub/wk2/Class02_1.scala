package org.sha0w.pub.wk2

import scala.math.abs
class Class02_1 {

}
object Class02_1 {
  val tolerance = 0.00001

  def isCloseEnogh(x:Double, y:Double): Boolean = abs((x-y)/x)/x < tolerance

  def fixedPoint(f:Double => Double)(firstGuess : Double) : Double = {
    def iterator(guess : Double): Double = {
      val next = f(guess)
      if (isCloseEnogh(next, guess)) next
      else iterator(next)
    }
    iterator(firstGuess)
  }

  def sqrt(x : Double):Double = {
    fixedPoint(averageDump(y => x / y))(1.5)
  }

  def main(args: Array[String]): Unit = {
    println(sqrt(2))
  }

  def averageDump(f:Double => Double)(x:Double) :Double = {
    (f(x) + x) / 2
  }
}
