package org.sha0w.pub.wk6
class Polynomials(terms0 : Map[Int, Double]) {
  val terms: Map[Int, Double] = terms0 withDefaultValue 0.0
  def + (other : Polynomials) : Polynomials = //update
    new Polynomials(terms ++ (other.terms map adjest))

  def +++ (other : Polynomials) : Polynomials = {
    new Polynomials(other.terms.foldLeft(terms)(addTerm))
  }
  def addTerm(terms : Map[Int,Double], term : (Int, Double)) : Map[Int, Double] = {
    terms withDefaultValue 0.0
    val value = terms(term._1)
    terms ++ Map(term._1 -> (term._2 + value))
  }
  override def toString: String = {
    terms.toList.sorted.reverse.map(a => a._2 + " x^" + a._1) mkString " + "
  }
  def adjest(term : (Int, Double)) : (Int, Double) = {
    val (exp , coff) = term
    exp -> (coff + terms(exp))
  }
}
object Polynomials {
  def main(args: Array[String]): Unit = {
    val p1 = new Polynomials(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
    val p2 = new Polynomials(Map(0->3.0, 3 -> 7.0))
    println(p1 +++ p2)
  }
}
