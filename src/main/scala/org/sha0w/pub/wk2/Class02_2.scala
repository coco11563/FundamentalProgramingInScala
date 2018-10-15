package org.sha0w.pub.wk2
class Class02_2 {

}
object Class02_2 {
  val c1 = new Rational(1,2)
  val c2 = new Rational(1,3)
  val c3: Rational = c1 - c2

  def main(args: Array[String]): Unit = {
    val te = c3 neg()
    println(te.toString)
    println(c3.toString)
    println((c1 * c2 + c1 * c2) .toString)
  }


  class Rational(x: Int, y : Int) {

    require( y != 0 , "the denominator can't be zero")
    require( y > 0 , "the denominator must be positive")

    def this(x : Int) = this(x, 1)

    private def gcd(a:Int, b:Int) : Int = {
      if (b == 0) a
      else gcd(b, a % b)
    }

    private val g = math.abs(gcd(x, y))
    //  println(g)
    def numer: Int = x / g
    def denom: Int = y / g

    def < (l : Rational) : Boolean = {
      numer * l.denom < l.numer * denom
    }

    def > (l : Rational) : Boolean = {
      !(this < l)
    }

    def max(l : Rational) : Rational = {
      if(this > l) this else l
    }

    def == (l : Rational) : Boolean = {
      numer * l.denom == l.numer * denom
    }

    def <= (l : Rational) : Boolean = {
      this < l || this == l
    }

    def >= (l : Rational) : Boolean = {
      this > l || this == l
    }

    def +(l:Rational) :Rational = {
      new Rational(numer * l.denom + denom * l.numer, denom * l.denom)
    }

    def - (l:Rational) : Rational = {
      this + l.neg()
    }

    def *(l: Rational) : Rational = {
      new Rational(numer * l.numer, denom * l.denom)
    }
    def neg() : Rational = {
      new Rational(-numer , denom)
    }
    override def toString: String = numer + "/" + denom
  }

}
