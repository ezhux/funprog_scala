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
    * Exercise 2.2
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

  /**
    * Exercise 2.3
    */
  def curry[A,B,C] (f: (A, B) => C): A => (B => C) = {
    (a: A) => {
      (b: B) => f(a,b)
    }
  }

  def curryShorter[A,B,C] (f: (A, B) => C): A => (B => C) = {
    a => b => f(a,b)
  }

  /**
    * Exercise 2.4
    */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  /**
    * Exercise 2.5
    */
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
      a => f(g(a))
  }

  /**
    * Exercise 3.2
    */
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case x :: Nil => Nil
    case x :: xs => xs
  }

  /**
    * Exercise 3.3
    */
  def setHead[A](l: List[A], a: A): List[A] = l match {
    case Nil => List(a)
    case x :: Nil => List(a)
    case x :: xs => a :: xs
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = n match {
    case x if (x > l.size) => sys.error("we don't have that many elements")
    case 0 => l
    case x => drop(l.tail, x - 1)
  }

  //3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    if (f(l.head)) {
      dropWhile(l.tail, f)
    }
    else
      l
  }

}