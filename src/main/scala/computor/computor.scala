package computor

import scala.util.{Try, Success, Failure}
import scala.math.pow
import scala.collection._

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
/* Basic Minimum Grammar
 * %%
 * Expr          ::= Term "+" Expr | Term "-" Expr | Term
 * Term          ::= Coeff " * " Indeterminate "^" Exponent
 * Coeff         ::= Number
 * Exponent      ::= Integer
 * Indeterminate ::= "X"
 * Number        ::= a
 */

case class Term(coefficient: Double, exponent: Int) {
  // TODO: throw error for trying to add terms of different exponents
  def +(rhs: Term): Term =
    Term(coefficient + rhs.coefficient, exponent)
  def unary_-(): Term = Term(-coefficient, exponent)
}

case class Parser(input: String) {
  def toPolynomial(): Try[Polynomial] = {
    Success(
      Polynomial(Seq(Term(5.0,0), Term(4.0,1), Term(-9.3,2)),
      Seq(Term(1.0, 0)))
    )
  }
}

case class Quadratic(a: Double, b: Double, c: Double) {
  def solve(): (Option[Double], Option[Double]) = {
    val + = (a:Double, b:Double) => a + b
    val - = (a:Double, b:Double) => a - b

    def aux(n: Double)(fn: (Double, Double) => Double): Double =
      (fn(-b, pow(n, 0.5)) / (2.0 * a))

    b * b - 4.0 * a * c match {
      case n if n > 0.0 => (
        Some(aux(n) (+)),
        Some(aux(n) (-)))
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
