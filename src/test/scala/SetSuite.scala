import org.scalatest.FunSuite
import computor._

class SetSuite extends FunSuite {
  test("Correct Calculations") {
    assert(new Quadratic(2, 5, -3).solve == (Some(0.5), Some(-3.0)))
  }
}
