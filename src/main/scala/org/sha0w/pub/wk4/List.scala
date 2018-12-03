package org.sha0w.pub.wk4

import java.util.NoSuchElementException

trait List[+T] {
  def isEmpty : Boolean
  def head : T
  def tail : List[T]
  def singleton[S](elem : S)= new Cons[S](elem, new Nil[S])
  def nth(index : Int) : T = {
    if (isEmpty || index < 0) throw new IndexOutOfBoundsException("wops you got a IOBE : " + index)
    else if (index == 0) head
    else tail nth index - 1
  }

  def prepend[U >: T](elem : U ) : List[U] = {
    new Cons(elem, this)
  }
}

class Cons[T](val head : T, val tail : List[T]) extends List[T] {
  override def isEmpty: Boolean = false
}

class Nil[T] extends List[T] {
  override def isEmpty: Boolean = true

  override def head: Nothing = throw new NoSuchElementException("hit the nil's head")

  override def tail: Nothing = throw new NoSuchElementException("hit the nil's tail")
}