package org.sha0w.pub.wk3

class Class03_01 {

}
object Class03_01 {
  def main(args: Array[String]): Unit = {
    println("hello world")

    val set1 = (new NonEmpty(3, Empty, Empty) incl 1) incl 4
    println(set1)
    println(set1 union new NonEmpty(2, Empty, Empty))
  }


  //persistent data structure > the change will never happen to the old version
  // <-- cornerstone of scaling functional programing up to collections
  abstract class IntSet { //super class  <--- trait
    def incl(x:Int):IntSet
    def contains(x:Int):Boolean
    def union(other:IntSet):IntSet
  }

  object Empty extends IntSet { //sub object <0 singleton object

    override def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

    override def contains(x: Int): Boolean = false

    override def toString: String = "."

    override def union(other: IntSet): IntSet = other
  }

  class NonEmpty(i: Int, left: IntSet, right: IntSet) extends IntSet{ //sub class

    override def incl(x: Int): IntSet = {
      if (i > x) new NonEmpty(i, left incl x, right)
      else if (i < x) new NonEmpty(i, left, right incl x)
      else this
    }

    override def contains(x: Int): Boolean = {
      if (i > x) left contains x
      if (i < x) right contains x
      else true
    }

    override def toString: String = "{" + left + i + right + "}"

    override def union(other: IntSet): IntSet = {
      ((right union left) union other) incl i
    }
  }
}
