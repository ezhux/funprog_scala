import org.scalatest.FunSuite
import mypack.fibonacci
/**
  * Created by tadassugintas on 2016-08-21.
  */
class FibonacciTest extends FunSuite {

  test("fib of 0") {
    assert(fibonacci.fib(0) == 0)
  }

  test("fib of 1") {
    assert(fibonacci.fib(1) == 1)
  }

  test("fib of 2") {
    assert(fibonacci.fib(2) == 1)
  }

  test("fib of 3") {
    assert(fibonacci.fib(3) == 2)
  }

  test("fib of 4") {
    assert(fibonacci.fib(4) == 3)
  }

  test("fib of 5") {
    assert(fibonacci.fib(5) == 5)
  }

  test("fib of 6") {
    assert(fibonacci.fib(6) == 8)
  }

  test("isSorted1") {
    val a = Array(1,2,3,4)
    assert(fibonacci.isSorted(a, (x: Int, y: Int) => x < y) == true)
  }

  test("isSorted2") {
    val a = Array(1,6,3,4)
    assert(fibonacci.isSorted(a, (x: Int, y: Int) => x < y) == false)
  }

  test("isSorted3") {
    val a = Array(2,4,8,16)
    assert(fibonacci.isSorted(a, (x: Int, y: Int) => x * x == y) == false)
  }

  test("isSorted4") {
    val a = Array(2,5,8,16)
    assert(fibonacci.isSorted(a, (x: Int, y: Int) => x * x == y) == false)
  }
}
