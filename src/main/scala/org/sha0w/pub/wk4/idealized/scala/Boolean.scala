package org.sha0w.pub.wk4.idealized.scala

import scala.language.postfixOps

abstract class Boolean  {
  def ifElseThen[T](e: => T , f: => T) : T

  def || (t: => Boolean) : Boolean = ifElseThen(True, t)

  def && (t: => Boolean) : Boolean = ifElseThen(t, False)

  def ! : Boolean = ifElseThen(False, True)

  def == (x : Boolean) : Boolean = ifElseThen(x, x!)

  def != (x : Boolean) : Boolean = ifElseThen(x!, x)

  def < (x : Boolean) : Boolean = ifElseThen(False, x) // cool
}

object True extends Boolean {
  override def ifElseThen[T](e: => T, f: => T): T = e
}
object False extends Boolean {
  override def ifElseThen[T](e: => T, f: => T): T = f
}
