import org.scalatest.FunSuite
import computor._

/*
5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0
5 * X^0 + 4 * X^1 = 4 * X^0
8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0
5 + 4 * X + X^2= X^2
1+4*X-X^2=0
9x^2+6x+3
*/

class SetSuite extends FunSuite {
  test("Correct Calculations") {
    assert(new Quadratic(2, 5, -3).solve == (Some(0.5), Some(-3.0)))
  }
  test("Poltnomial") {
    val l = Seq(Term(3, 0), Term(6, 1), Term(3, 2))
    val r = Seq(Term(0, 0), Term(0, 1), Term(0, 2))
    println(Polynomial(l, r).simplify.solve)
  }
}
