package computor

import scala.util.{Try, Success, Failure}
import scala.math.pow

/*
 * variables
 * operators
 * expression
 * equals
 * polynomial
 */

case class Term(coefficient: Double, exponent: Int)

class Parser(input: String) {
  def toPolynomial(): Try[Polynomial] = ???
}

class Quadratic(a: Double, b: Double, c: Double) {
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

class Polynomial(lhs: List[Term], rhs: List[Term]) {
  // Reduce polynomial and send to Quadratic to solve
  def solve(): (Option[Double], Option[Double]) = ???
}
