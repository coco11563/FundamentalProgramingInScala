package org.sha0w.pub.wk5

class OnClassTest {

}
object OnClassTest {
  def init[T](xs : List[T]) : List[T] = xs match {
    case List() => throw new Error("init of Empty List")
    case List(x) => List()
    case y :: ys => y :: init(xs)
  }

  def reverse_1[T](xs : List[T]) : List[T] = xs match {
    case List() => xs
    case y :: ys => reverse_1(ys) ++ List(y)
  }
  def reverse_2[T](xs : List[T]) : List[T] = xs match {
    case List() => xs
    case y :: ys => reverse_2(ys).++(List(y))
  }

//  def removeAt[T](n: Int, xs: List[T]) : List[T] = n match {
//    case 0 => xs.tail
//    case _ => xs.head :: removeAt(n - 1, xs.tail)
//  }
  def removeAt[T](n: Int, xs: List[T]) : List[T] = (xs take n) ::: ( xs drop n + 1)
  def main(args: Array[String]): Unit = {
    reverse_1(List(1,2,3,4)).foreach(println(_))
    reverse_2(List(1,2,3,4)).foreach(println(_))
    removeAt(1, List(1,2,3,4)).foreach(println(_))
    flatten(List(List(1, 1), 2, List(3, List(5, 8)))).foreach(print)

  }

  def flatten(xs: List[Any]): List[Any] = xs match {
    case List() => xs
    case y :: ys => y :: flatten(ys)
  }
//  def squareList(xs: List[Int]): List[Int] =
//    xs match {
//      case Nil => Nil
//      case y :: ys => y* y :: squareList(ys)
//    }
////
//  def squareList(xs: List[Int]): List[Int] =
//    xs map (x => x * x)
}
