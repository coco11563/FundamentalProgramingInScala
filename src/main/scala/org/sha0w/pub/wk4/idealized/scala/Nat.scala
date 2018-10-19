package org.sha0w.pub.wk4.idealized.scala
//the implementation of Peano numbers <- Integer & Float can build from this
abstract class Nat { // can be negative
  def isZero : Boolean
  def predecessor : Nat // positive nature number before
  def successor : Nat //positive nature number after
  def + (that : Nat) : Nat //addition
  def - (that : Nat) : Nat //subtraction
}
object Zero extends Nat {

  override def isZero: Boolean = True

  override def predecessor: Nat = throw new IllegalStateException()

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat = that.isZero.ifElseThen(this, throw new IllegalArgumentException("there is no negative NATURE NUMBER"))
}

class Succ(n : Nat) extends Nat {

  override def isZero: Boolean = False

  override def predecessor: Nat = n

  override def successor: Nat = new Succ(this)

  override def +(that: Nat): Nat = new Succ(n + that) //that.isZero.ifElseThen(this, this.successor + that.predecessor)

  override def -(that: Nat): Nat = that.isZero.ifElseThen(this, n - that.predecessor)
}
