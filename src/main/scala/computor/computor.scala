package computor

import scala.util.{Try, Success, Failure}
import scala.math.{pow, Ordered}
import scala.util.Sorting
import scala.collection._
import scala.util.parsing.combinator._
import scala.util.parsing.input.{Position, Positional}

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

case class Term(coefficient: Double, exponent: Int) extends Ordered[Term] {
  def compare(that: Term) = this.exponent - that.exponent
  // TODO: throw error for trying to add terms of different exponents
  def +(rhs: Term): Term =
    Term(coefficient + rhs.coefficient, exponent)
  def unary_-(): Term = Term(-coefficient, exponent)
  def longString = coefficient+" * X^"+exponent
}

// TODO: move parser to separate file
object PolynomialLexer extends JavaTokenParsers {
  override def skipWhitespace = true

  def apply(input: String): Either[NoSuccess, Polynomial] = parseAll(polynomial, input) match {
    case failure : NoSuccess => Left(failure)
    case Success(result, _) => Right(result)
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
  def degree(): Int = (lhs.isEmpty, rhs.isEmpty) match {
    case (false, false) => Seq(lhs.max, rhs.max).max.exponent
    case (true, false) => rhs.max.exponent
    case (false, true) => lhs.max.exponent
    case _ => 0
  }
  def simplify(): Polynomial = {
    val newleft = (lhs ++ rhs.map((t: Term) => -t))
    Polynomial(newleft.groupBy((t: Term) => t.exponent)
      .map({ case (k, v) => (k, v.reduceLeft(_ + _)) })
      .values.toList.sorted, Seq())
  }
  // Reduce polynomial and send to Quadratic to solve
  def solve(): (Option[Double], Option[Double]) = {
    val s = simplify
    val v = s.lhs.reverse
    val r = v.map(_.coefficient)
    degree match {
      case 0 => (Some(r(0)), None)
      case 1 => (Some(-r(1) / r(0)), None)
      case 2 => Quadratic(r(0), r(1), r(2)).solve
      case n => ??? // Failure(s"Expected degree < 3, found $n")
    }
  }

  def exprString(expr: Seq[Term]) = expr.isEmpty match {
    case false => expr.head.longString+expr.tail.map{x => " + "+x.longString}.mkString
    case true => "0"
  }
  def longString = exprString(lhs)+" = "+exprString(rhs)
}
