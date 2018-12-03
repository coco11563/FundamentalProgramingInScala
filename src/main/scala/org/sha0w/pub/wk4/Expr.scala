package org.sha0w.pub.wk4

trait Expr {
  def isNumber : Boolean
  def isSum : Boolean
  def numValue : Int
  def leftOP : Expr
  def rightOP : Expr
}

class Number(n : Int) extends Expr {
  override def isNumber: Boolean = true

  override def isSum: Boolean = false

  override def numValue: Int = n

  override def leftOP: Expr = throw new Error("num.leftOP")

  override def rightOP: Expr = throw new Error("num.rightOP")
}


class Sum(l:Expr, r:Expr) extends Expr {
  def left : Expr = l

  def right : Expr = r

  override def isNumber: Boolean = false

  override def isSum: Boolean = true

  override def numValue: Int = throw new Error("sum.numValue")

  override def leftOP: Expr = left

  override def rightOP: Expr = right

  def eval(expr: Expr) : Int = {
    if (expr.isNumber) expr.numValue
    else if (expr.isSum) eval(expr.leftOP) + eval(expr.rightOP)
    else throw new Error("Unknown Expr")
  }
}