import org.scalatest.funsuite.AnyFunSuite
import computor._

/*
5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0
5 * X^0 + 4 * X^1 = 4 * X^0
8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0
5 + 4 * X + X^2= X^2
1+4*X-X^2=0
9x^2+6x+3
*/


class SetSuite extends AnyFunSuite {
  test("Correct Calculations") {
    assert(new Quadratic(2, 5, -3).solve == (Some(0.5), Some(-3.0)))
  }
  test("Poltnomial") {
    val l = Seq(Term(3, 0), Term(6, 1), Term(3, 2))
    val r = Seq(Term(0, 0), Term(0, 1), Term(0, 2))
    println(Polynomial(l, r).simplify.solve)
  }
  // test("Parser") {
  //   case class TermTest(coeff: Double, exponent: Int)
  //   sealed trait TermToken extends Positional
  //   case class NUMBER(str: String) extends TermToken
  //   case class ADDTERM(str: String) extends TermToken
  //   case class SUBTERM(str: String) extends TermToken
  //   case class NEGTERM(str: String) extends TermToken
  //   object PolynomialLexer extends JavaTokenParsers {
  //     override def skipWhitespace = true
  //     def number: Parser[Double] = positioned { floatingPointNumber ^^ { str => NUMBER(str) } }
  //     def poly: Parser[POLY] = positioned { expr ~ opt("=" ~ expr) ^^ { str => POLY(str) } }
  //     def expr: Parser[EXPR] = positioned { term ~ rep("+" ~ term | "-" ~ term) ^^ { str => EXPR(str) } }
  //     def term: Parser[TERM] = positioned { coef ~ "*" ~ varb ^^ { str => TERM(str) } }
  //     def varb: Parser[VARB] = positioned { "X^" ~ wholeNumber ^^ { str => VARB(str) } }
  //     def tokens: Parser[List[WorkflowToken]] = {
  //       phrase(poly) ^^ { rawTokens => }
  //     }
  //   }
  // }
  test("LexerParser") {
    val p: PolynomialLexer = new PolynomialLexer()
    println(p.parse(p.polynomial, "5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0"))
    println(p.parse(p.polynomial, "5 * X^0 + 4 * X^1 = 4 * X^0"))
    println(p.parse(p.polynomial, "8 * X^0 - 6 * X^1 + 0 * X^2 - 5.6 * X^3 = 3 * X^0"))
    println(p.parse(p.polynomial, "5 + 4 * X + X^2= X^2"))
    println(p.parse(p.polynomial, "1+4*X-X^2=0"))
    println(p.parse(p.polynomial, "9x^2+6x+3"))
  }
}
