package org.sha0w.pub.futureTest

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
class test1 extends App {
  Future {
    Thread.sleep(1000)
  }
}
