package computor

// import scala.util.{Try, Success, Failure}
import scala.math.pow

// TODO: Consider complex solutions
//       Also clean up gross kludge
case class FindRoots(t: Seq[Term], deg: Int) {
  // TODO: change method name
  def salve: (Option[Double], Option[Double]) = deg match {
    case 0 => tryConstant
    case 1 => tryLinear
    case 2 => tryQuadratic
    case _ => (None, None)
  }
  def tryConstant: (Option[Double], Option[Double]) = (Some(t(0).coeff), None)
  def tryLinear: (Option[Double], Option[Double]) = t.length match {
    case 1 => (Some(0), None)
    case 2 => (Some(-t(1).coeff / t(0).coeff), None)
    case _ => (None, None)
  }
  def tryQuadratic: (Option[Double], Option[Double]) = t.length match {
    case 1 => SolveQuadratic(t(0).coeff, 0, 0)
    case 2 => t(1).exp match {
      case 0 => SolveQuadratic(t(0).coeff, 0, t(1).coeff)
      case 1 => SolveQuadratic(t(0).coeff, t(1).coeff, 0)
      case _ => (None, None)
    }
    case 3 => SolveQuadratic(t(0).coeff, t(1).coeff, t(2).coeff)
    case _ => (None, None)
  }
}

// TODO: fix for x^2+2=0
object SolveQuadratic {
  def apply(a: Double, b: Double, c: Double): (Option[Double], Option[Double]) = {
    b * b - 4.0 * a * c match {
      case n if n > 0.0 => (
        Some((-b + pow(n, 0.5)) / (2.0 * a)),
        Some((-b - pow(n, 0.5)) / (2.0 * a)))
      case 0 => (
        Some((-b / (2.0 * a))),
        None)
      case _ => (None, None) // TODO: account for complex roots, negative discriminant
    }
  }
}

object ComputorV1 {
  def apply(input: String) = ???
}
