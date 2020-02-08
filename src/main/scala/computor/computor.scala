package computor

import scala.util.{Try, Success, Failure}
import scala.math.pow
import scala.collection._
import scala.util.parsing.combinator._
import scala.util.parsing.input.Positional

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

case class Term(coefficient: Double, exponent: Int) {
  // TODO: throw error for trying to add terms of different exponents
  def +(rhs: Term): Term =
    Term(coefficient + rhs.coefficient, exponent)
  def unary_-(): Term = Term(-coefficient, exponent)
}

// TODO: move parser to separate file
class PolynomialLexer extends JavaTokenParsers {
  override def skipWhitespace = true
  def polynomial: Parser[Polynomial] = {
    ((expression) ~ "=" ~ (expression)) ^^ {
      case lhs ~ equ ~ rhs => Polynomial(lhs, rhs)
    }
  }
  def expression: Parser[List[Term]] = (rep1(term))
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
  def term: Parser[Term] = {
    ((coefficient) ~ "*" ~ (exponent)) ^^ {
      case c ~ op ~ p => Term(c, p)
    }
  }
  def variable: Parser[String] = "[A-Za-z]".r
  // def toPolynomial(s: String): Try[Polynomial] = {
  //   apply(s) match {
  //     case (lhs, rhs) => Success(Polynomial(lhs, rhs))
  //   }
  // }
}

case class Quadratic(a: Double, b: Double, c: Double) {
  def solve(): (Option[Double], Option[Double]) = {
    b * b - 4.0 * a * c match {
      case n if n > 0.0 => (
        Some((-b + pow(n, 0.5)) / (2.0 * a)),
        Some((-b - pow(n, 0.5)) / (2.0 * a)))
      case 0 => (
        Some((-b / (2.0 * a))),
        None)
      case _ => (None, None)
    }
  }
}

case class Polynomial(lhs: Seq[Term], rhs: Seq[Term]) {
  def combine(x: (Term, Term, Term), y: Term) = {
    y.exponent match {
      case 0 => (x._1 + y, x._2, x._3)
      case 1 => (x._1, x._2 + y, x._3)
      case 2 => (x._1, x._2, x._3 + y)
    }
  }
  def simplify(): Polynomial = {
    val newleft = (lhs ++ rhs.map((t: Term) => -t))
    val l = newleft.foldLeft((Term(0, 0), Term(0, 1), Term(0, 2)))(combine)
    Polynomial(Seq(l._1, l._2, l._3), Seq())
  }
  // Reduce polynomial and send to Quadratic to solve
  def solve(): (Option[Double], Option[Double]) = {
    val s = simplify
    val v = s.lhs.reverse
    val r = v.map(_.coefficient)
    Quadratic(r(0), r(1), r(2)).solve
    // Quadratic(s.lhs.reverse.map(_.coefficient)).solve
  }
}
