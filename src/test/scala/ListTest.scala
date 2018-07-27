import fpinscala.datastructures.{List, Nil}
import org.scalatest.FunSuite
/**
  * Created by tadassugintas on 24/07/2017.
  */
class ListTest extends FunSuite {

  test("tail") {
    val a = List(1,2,3,4)
    assert(List.tail(a) == List(2,3,4))
  }

  test("setHead") {
    val a = List(1,2,3,4)
    assert(List.setHead(a, 9) == List(9, 2,3,4))
  }

  test("setHeadEmpty") {
    val a = Nil
    assert(List.setHead(a, 9) == List(9))
  }

  test("setHeadSingleElem") {
    val a = List(4)
    assert(List.setHead(a, 9) == List(9))
  }

  test("drop") {
    val l = List(1,2,3,4,5,6,7)
    assert(List.drop(l, 3) == List(4,5,6,7))
  }

  test("drop more than we have") {
    assertThrows[RuntimeException] {
      (List.drop(List(1), 3) == List(4,5,6,7))
    }
  }

  test("drop while") {
    val l = List(1,2,3,4,5,6,7)
    assert(List.dropWhile(l, (x: Int) => x < 5) == List(5,6,7))
  }

  test("test init") {
    val l = List(1,2,3,4)
    assert(List.init(l) == List(1,2,3))
  }

  test("test length") {
    val l = List(1,2,3,4,3)
    assert(List.length(l) == 5)
  }

  test("test length zero") {
    val l = List[Int]()
    assert(List.length(l) == 0)
  }

  test("test length 10") {
    val l = List(1,2,3,4,5,1,2,3,4,5)
    assert(List.length(l) == 10)
  }

  test("test reverse") {
    val l = List(1,2,3,4,5)
    assert(List.reverse(l) == List(5,4,3,2,1))
  }

  test("test addOne") {
    val l = List(1,3,4,5)
    assert(List.addOne(l) == List(2,4,5,6))
  }
}
