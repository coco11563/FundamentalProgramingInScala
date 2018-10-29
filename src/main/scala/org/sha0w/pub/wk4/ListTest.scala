package org.sha0w.pub.wk4

import java.util.NoSuchElementException

trait List[T] {
  def isEmpty : Boolean
  def head : T
  def tail : List[T]
  def singleton[S](elem : S)= new Cons[S](elem, new Nil[S])
  def nth(index : Int) : T = {
    if (isEmpty || index < 0) throw new IndexOutOfBoundsException("wops you got a IOBE : " + index)
    else if (index == 0) head
    else tail nth index - 1
  }
}
object List {
  def apply[T](): List[T] = new Nil[T]()

  def apply[T](t: T*): List[T] = {
    var l : List[T] = List()
    for (t_ <- t) {
      l = new Cons[T](t_, l)
    }
    l
  }
  def main(args: Array[String]): Unit = {
    val c1 = new Cons[Int](1, new Cons[Int](2, new Cons[Int](3, new Nil)))
    val c2 = List[Int](7,6,5,4,3,2,1)
    println(c2.nth(1))
//    println(c1.nth(-1))
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