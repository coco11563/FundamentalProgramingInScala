
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

val fut = Future {
  Thread.sleep(1000) ; 21 + 21
}

fut.isCompleted

fut.value

val a = Array(1,2,3)
a.splitAt(2)._1

//n
//def twoSum(nums: Array[Int], target: Int): Array[Int] = {
//  for (i <- nums.indices) {
//    if (nums(i) < target) {
//      println(nums(i), target)
//      val j = search(nums, target - nums(i), i)
//      if (j != -1) return Array(i, j)
//    }
//  }
//  Array()
//}

//n
def search(nums : Array[Int], target : Int, start : Int) : Int = {
  for (i <- start until nums.length) {
    println(s"::$target, ${nums(i)}")
    if (nums(i) == target) return i
  }
  -1
}
def twoSum(nums: Array[Int], target: Int): Array[Int] = {
  import scala.collection.mutable
  val map = new mutable.HashMap[Int, Int]
  for (i <- nums.indices) {
    map.put(nums(i), i)
  }
  for (i <- nums.indices) {
    val j = map.get(target - nums(i))
    if (! (j eq None)) return Array(i, j.get)
  }
  Array()
}
twoSum(Array(2,7,11,15),13)

def smallestRangeII(A: Array[Int], K: Int): Int = {
  if (A.length == 0) return 0
  val ave = average(A)
  val arr = A.map(i => {
    i + nearest(i, ave, K)
  })
  arr.max - arr.min
}

def average(A : Array[Int]) : Double = {
  var i = 0
  A.foreach(num => {
    i += num
  })
  i / A.length
}

def nearest(i : Int, target : Double, K : Int) : Int = {
  if (i > target) {
    - 1 * K
  } else { // i >= target
    K
  }
}