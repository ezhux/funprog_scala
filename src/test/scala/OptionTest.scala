import org.scalatest.FunSuite

import scala.{Option => _, Some => _, Either => _, _}
/**
  * Created by tadassugintas on 28/07/2017.
  */
class OptionTest extends FunSuite {

  test("map test") {

    assert(4 == Some(2).map((x:Int) => x * x).get)
  }

  test("map test none") {
    assert(None == None.map((x:Int) => x * x))
  }

  test("get or else") {
    assert(Some(2).getOrElse(1) == 2)
  }

  test("get or else none ") {
    assert(None.getOrElse(1) == 1)
  }

  test("flatMap ") {
    assert(Some(1).flatMap{x => Some(x+1)} == Some(2))
    assert(None.flatMap{x:Int => Some(x+2)} == None)
  }

  test("or else ") {
    assert(Some(1).orElse(Some(100)) == Some(1))
    assert(None.orElse(Some(100)) == Some(100))
  }

  test("filter") {
    assert(Some(1).filter((x: Int) => x > 2) == None)
    assert(Some(10).filter((x: Int) => x > 2) == Some(10))
  }


  test("map2") {
    assert(fpinscala.errorhandling.Option.map2(fpinscala.errorhandling.Some(1), fpinscala.errorhandling.Some("123"))((x, y ) => x + y.length) == fpinscala.errorhandling.Some(4))
    assert(fpinscala.errorhandling.Option.map2(fpinscala.errorhandling.Some(1), fpinscala.errorhandling.None)((x, y) => x + y) == fpinscala.errorhandling.None)
    assert(fpinscala.errorhandling.Option.map2(fpinscala.errorhandling.None, fpinscala.errorhandling.Some(100))((x: Int, y: Int) => x + y) == fpinscala.errorhandling.None)
    assert(fpinscala.errorhandling.Option.map2(fpinscala.errorhandling.None, fpinscala.errorhandling.None)((x: Double, y: Double) => x + y) == fpinscala.errorhandling.None)
//    Option.map2(Some(1), None)((x: Int,y: Int) => x + y)
//    assert(Option.map2((a:Some(1), b:Some(4))((x: Int, y: Int) => x + y)) == Some(5))
  }
}
