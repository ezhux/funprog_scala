package mypack
import scala.annotation.tailrec

/**
  * Created by tadassugintas on 2016-08-21.
  */
object fibonacci {

/*
  Exercise 2.1
 */
  def fib(x: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, curr: Int): Int = {
      if (n == 0)
       prev
      else
        go(n - 1, curr,  prev + curr)
    }
    go(x, 0, 1)
  }

  /**
    * Exercise 2.1
    * @param as
    * @param ordered
    * @tparam A
    * @return
    */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {

    @tailrec
    def loop(n: Int): Boolean = {
      if(n > as.length - 1)
        true
      else
        if (ordered(as(n-1), as(n)))
          loop(n+1)
        else
          false
    }
    loop(1)
  }
}