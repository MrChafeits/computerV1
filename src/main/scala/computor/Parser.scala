package computor

import scala.util.parsing.combinator.JavaTokenParsers

/*
 * input string: "5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0"
 * variables: X
 * operators: + -
 * expression: ???
 * equals: lhs = rhs
 * polynomial: lhs["5 * X^0", "4 * X^1", "-9.3 * X^2"] equals rhs["1 * X^0"]
 * lhs[Term(5.0, 0), Term(4.0, 1), Term(-9.3, 2)] equals rhs[Term(1, 0)]
 */
/* A parser for things
 * Is a function from strings
 * To lists of pairs
 * Of things and strings
 */
/* -- Requirements --
 *  solve polynomial equations of degree <= 2
 *  display reduced form of equation with valid degree
 *  the solution as well as the sign of the discriminant when necessary
 * -- Explicit Bonuses --
 *
 * Basic Minimum Grammar
 * %%
 * expr     ::= term \{"+" term | "-" term\}
 * term     ::= factor \{"*" factor | "/" factor\}
 * factor   ::= floatingPointNumber
 * exponent ::= variable "^" [0-2]
 * variable ::= "X"
 * %%old
 * Expr     ::= Term " + " Expr | Term " - " Expr | Term
 * Term     ::= Coeff " * " Variable "^" Exponent
 * Coeff    ::= Number
 * Exponent ::= Integer
 * Variable ::= "X"
 * Number   ::= a
 */

object PolynomialLexer extends JavaTokenParsers {
  override def skipWhitespace = true

  def apply(input: String): Polynomial = parseAll(polynomial, input) match {
    case failure : NoSuccess => throw new IllegalArgumentException(failure.toString)
    case Success(result, _) => result
  }
  def polynomial: Parser[Polynomial] = {
    ((expression) ~ "=" ~ (expression)) ^^ {
      case lhs ~ equ ~ rhs => Polynomial(lhs, rhs)
    }
  }
  def expression: Parser[List[Term]] = (rep1(term))
  def term: Parser[Term] = {
    ((coefficient) ~ "*" ~ (exponent)) ^^ {
      case c ~ op ~ p => Term(c, p)
    }
  }
  def coefficient: Parser[Double] = {
    (opt("[+-]".r) ~ decimalNumber) ^^ {
      case Some(op) ~ num => {
        val coef: Double = num.toDouble
        if (op == "-")
          -coef
        else
          coef
      }
      case None ~ num => num.toDouble
    }
  }
  def exponent: Parser[Int] = {
    (variable ~ "^" ~ wholeNumber) ^^ {
      case v ~ e ~ n => n.toInt
    }
  }
  def variable: Parser[String] = "[A-Za-z]".r
}
