import org.scalatest.FunSuite
import state._

class StateTest extends FunSuite {

  test("NonNegativeInt") {
    val seed = SimpleRNG(42)
    val (i, r) = State.nonNegative(seed)
    assert (i > 0)
  }

  test("double") {
    val seed = SimpleRNG(42)
    val (i, r) = State.double(seed)
    assert (i >= 0 && i < 1)
  }

  test("ints") {
    val seed = SimpleRNG(42)
    val r = State.ints(10)(seed)
    Console.println(r._1)
    assert(r._1.length == 10)
  }

}
