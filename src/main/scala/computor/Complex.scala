package computor

import scala.math.{pow, sqrt, Ordered}

case class Complex(real: Double, imag: Double) extends Ordered[Complex] {
  private val modulus = sqrt(pow(real, 2) + pow(imag, 2))

  def this(real: Double) = this(real, 0)

  def unary_+ = this
  def unary_- = new Complex(-real, -imag)
  def unary_~ = new Complex(real, -imag)
  def unary_! = modulus

  def compare(that: Complex) = !this compare !that

  def +(rhs: Double): Complex = new Complex(real + rhs, imag)
  def -(rhs: Double): Complex = new Complex(real - rhs, imag)
  def *(rhs: Double): Complex = new Complex(real * rhs, imag * rhs)
  def /(rhs: Double): Complex = new Complex(real / rhs, imag / rhs)

  def +(b: Complex) = new Complex(real + b.real, imag + b.imag)
  def -(b: Complex) = new Complex(real - b.real, imag - b.imag)
  def *(b: Complex) = new Complex(real * b.real - imag * b.imag, imag * b.real + real * b.imag)
  def /(b: Complex) = {
    require(b.real != 0 || b.imag != 0)
    val d = pow(b.real, 2) + pow(b.imag, 2)
    new Complex((real * b.real + imag * b.imag) / d, (imag * b.real - real * b.imag) / d)
  }

  override def toString: String = imag match {
    case -1 => s"($real - ${-imag}i)"
    case _ => s"($real + ${imag}i)"
  }
}

object Complex {
  import scala.language.implicitConversions
  val i = new Complex(0, 1)

  def apply(real: Double) = new Complex(real)

  implicit def fromDouble(d: Double) = new Complex(d)
  implicit def fromFloat(f: Float) = new Complex(f)
  implicit def fromLong(l: Long) = new Complex(l)
  implicit def fromInt(i: Int) = new Complex(i)
  implicit def fromShort(s: Short) = new Complex(s)
}
