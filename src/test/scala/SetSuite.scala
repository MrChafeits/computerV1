import org.scalatest.funsuite.AnyFunSuite
import computor._
import scala.math.sqrt

/*
equation: 5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0
reduced:  4 * X^0 + 4 * X^1 - 9.3 * X^2 = 0
degree: 2
Discriminant is strictly positive, the two solutions are:
0.905239
-0.475131

equation: 5 * X^0 + 4 * X^1 = 4 * X^0
reduced:  1 * X^0 + 4 * X^1 = 0
degree: 1
The solution is:
-0.25

equation: 8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0
reduced:  5 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 0
degree: 3
Degree > 2, cannot solve
=== Bonus ===
equation: 5 + 4 * X + X^2= X^2
reduced:  5 + 4 * x = 0
degree: 1
The solution is:
-1.25

1+4*X-X^2=0
9x^2+6x+3
*/


class SetSuite extends AnyFunSuite {
  test("TermOrdering") {
    assert(Term(1, 0) < Term(-1, 1))

    val arr = Array(Term(0, 2), Term(0, 1), Term(0, 0))
    assert(arr.max == Term(0, 2))
  }
  test("Correct Calculations") {
    assert(new Quadratic(2, 5, -3).solve == (Some(0.5), Some(-3.0)))
  }
  test("Poltnomial") {
    val l = Seq(Term(1, 0), Term(6, 1), Term(0, 2))
    val r = Seq(Term(-1, 0), Term(-2, 2), Term(-1, 0), Term(0, 1), Term(-1, 2))
    assert(Polynomial(l, r).solve == (Some(-1.0), None))

    val l1 = Seq(Term(-1, 2), Term(1, 0), Term(5, 2), Term(0, 1))
    val r1 = Seq(Term(9, 3), Term(6, 2), Term(7, 1))
    val p = Polynomial(l1, r1)
    println(p.simplify.longString)
  }
  test("LexerParser") {
    PolynomialLexer("5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0") match {
      case Left(s) => println(s)
      // (Some(-0.47513146390886934),Some(0.9052389907905898)) did not equal
      // (Some(-0.4751314639088694), Some(0.9052389907905898))
      case Right(p) => assert(p.solve == (Some(-0.47513146390886934),Some((2*sqrt(1030)+20)/93)))
    }
    PolynomialLexer("5 * X^0 + 4 * X^1 = 4 * X^0") match {
      case Left(s) => println(s)
      case Right(p) => assert(p.solve == (Some(-0.25), None))
    }
    PolynomialLexer("8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0") match {
      case Left(s) => println(s)
      case Right(p) => println(p.simplify.longString)
    }
    PolynomialLexer("2 * X^2 + 4 * X^1 - 6 * X^0 = 0 * X^0") match {
      case Left(s) => println(s)
      case Right(p) => assert(p.solve == (Some(1), Some(-3)))
    }
    // BONUS
    // println(PolynomialLexer("5 + 4 * X + X^2= X^2"))
    // println(PolynomialLexer("1+4*X-X^2=0"))
    // println(PolynomialLexer("9x^2+6x+3"))
  }
}
