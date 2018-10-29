package org.sha0w.pub.wk4

object Expr {
  def eval(expr: Expr) : Int = expr match {
    case sum(e1, e2) => eval(e1) + eval(e2)
    case prod(e1, e2) => eval(e1) * eval(e2)
    case number(e) =>  e
    case _ => throw new Error
  }

  def show(expr: Expr) : String = expr match {
    case sum(e1, e2) => "(" + show(e1) + "+" + show(e2) + ")"
    case prod(e1, e2) => show(e1) + "*" + show(e2)
    case Var(e) => e
    case number(e) => String.valueOf(e)
    case _ => throw new Error
  }


  def main(args: Array[String]): Unit = {
    println(eval(sum(number(1), number(2))))
    println(show(sum(number(1), number(2))))
    println(show(prod(sum( number(2), Var("x")), Var("y"))))
  }
}

trait Expr
case class sum (e1:Expr, e2:Expr) extends Expr
case class number (e:Int) extends Expr
case class Var(string: String) extends Expr {}
case class prod(e1 : Expr, e2 : Expr) extends Expr

