package computor

import scala.util.Sorting
import scala.collection.mutable.ArrayBuilder

// TODO: fix for 0 coeffs
case class Polynomial(lhs: Seq[Term], rhs: Seq[Term]) {
  def degree: Int = (lhs.isEmpty, rhs.isEmpty) match {
    case (false, false) => Seq(lhs.max, rhs.max).max.exp
    case (true, false) => rhs.max.exp
    case (false, true) => lhs.max.exp
    case _ => 0
  }

  // Change to apply automatically?
  def simplify(): Polynomial = {
    val newleft = (lhs ++ rhs.map((t: Term) => -t))
    Polynomial(newleft.groupBy((t: Term) => t.exp)
      .map({ case (k, v) => (k, v.reduceLeft(_ + _)) })
      .values.toList.sorted, Seq())
  }

  // Reduce polynomial and send to Quadratic to solve
  def solve(): (Option[Double], Option[Double]) = {
    val s = simplify
    val v = s.lhs.reverse
    val r = v.map(_.coeff)
    FindRoots(v, s.degree).salve
  }

  def isZero: Boolean = lhs.isEmpty && rhs.isEmpty

  override def toString: String =
    if (isZero) {
      "(0)"
    } else {
      val bldr = ArrayBuilder.make[Term]
      simplify.lhs.foreach((t: Term) => bldr += t)

      val ts = bldr.result()
      Sorting.quickSort(ts)
      val s = ts.reverse.mkString
      "(" + (if (s.take(3) == " - ") "-" + s.drop(3) else s.drop(3)) + ")"
    }

  def exprString(expr: Seq[Term]) = expr.isEmpty match {
    case false => expr.head.longString+expr.tail.map{x => " + "+x.longString}.mkString
    case true => "0"
  }

  def longString = exprString(lhs)+" = "+exprString(rhs)

  def solutionString(deg: Int, s: Polynomial): String = deg match {
    case 0 => s.solve match {
      case (Some(l), None) => if (l == 0) "All real numbers\n" else "No solution\n"
      case _ => "Error: case 0 => case _ reached in solutionString\n"
    }
    case 1 => s.solve match {
      case (Some(l), None) => s"Solution: $l\n"
      case _ => "Error: case 1 => case _ reached in solutionString\n"
    }
    case 2 => s.solve match {
      case (Some(l), Some(r)) => s"Positive discriminant with solutions: ($l, $r)\n"
      case (Some(l), None) => s"Solution: $l\n"
      case _ => s"Error: case 2 => case _ reached in solutionString\n"
    }
    case _ => s"Polynomial degree is greater than 2, cannot currently solve\n"
  }

  def reportString(): String = {
    val s = simplify
    val d = s.degree
    s"Reduced form: ${s.longString}\n"+
    s"Polynomial degree: $d\n"+
    solutionString(d, s)
  }
}
