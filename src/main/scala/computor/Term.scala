package computor

// Univariate polynomial term
case class Term(coeff: Double, exp: Int) extends Ordered[Term] {
  def compare(that: Term) = exp compare that.exp
  // TODO: throw error for trying to add terms of different exps
  def +(rhs: Term): Term = {
    if (exp != rhs.exp)
      throw new IllegalArgumentException(s"cannot add terms of degree $exp and ${rhs.exp}")
    Term(coeff + rhs.coeff, exp)
  }
  def unary_-(): Term = Term(-coeff, exp)

  def longString = coeff+" * X^"+exp
  override def toString: String = {
    import Term._
    def expString = exp match {
      case 0 => ""
      case 1 => "x"
      case _ => "x" + exp.toString.map(superscript)
    }
    def simpleCoeff: Option[String] = coeff match {
      case 0 => Some("")
      case 1 if exp == 0 => Some(s" + $coeff")
      case 1 => Some(s" + $expString")
      case -1 if exp != 0 => Some(s" - $expString")
      case _ => None
    }
    def stringCoeff: Option[String] = coeff.toString match {
      case IsZero() => Some("")
      case IsNegative(posPart) if exp == 0 => Some(s" - $posPart")
      case IsNegative(posPart) => Some(s" - $posPart$expString")
      case _ => None
    }
    simpleCoeff orElse stringCoeff getOrElse s" + $coeff$expString"
  }
}

object Term {
  private val IsZero = "0".r
  private val IsNegative = "-(.*)".r

  private val digitToSuperscript = Array(
    ('0', '\u2070'),
    ('1', '\u00B9'),
    ('2', '\u00B2'),
    ('3', '\u00B3'),
    ('4', '\u2074'),
    ('5', '\u2075'),
    ('6', '\u2076'),
    ('7', '\u2077'),
    ('8', '\u2078'),
    ('9', '\u2079'),
    ('-', '\u207B'),
    ('i', '\u2071'))

  private val superscriptRegex =
    """\u2070\u00B9\u00B2\u00B3\u2074\u2075\u2076\u2077\u2078\u2079\u207B\u2071""".r

  private[computor] def removeSuperscript(text: String): String =
    superscriptRegex.replaceAllIn(text, "^" + _.group(0).map(removeSuperscript))

  private val superscript : (Char => Char) = Map(digitToSuperscript:_*)
  private val removeSuperscript : (Char => Char) = Map(digitToSuperscript.map(_.swap):_*)
}
