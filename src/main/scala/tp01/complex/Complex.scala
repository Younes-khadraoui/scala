package tp01.complex

import scala.math.*

final class Complex(val real: Double, val imag: Double)
  extends Ordered[Complex]:

  override def toString: String =
    (real, imag) match
      case (0, 0) => "0"
      case (0, 1) => "i"
      case (0, -1) => "-i"
      case (0, i) => s"${i}i"
      case (r, 0) => s"$r"
      case (r, 1) => s"$r + i"
      case (r, -1) => s"$r - i"
      case (r, i) if i > 0 => s"$r + ${i}i"
      case (r, i) => s"$r - ${-i}i"

  def mod: Double =
    hypot(real, imag)

  def arg: Double =
    atan2(imag, real)

  def +(that: Complex): Complex =
    Complex(real + that.real, imag + that.imag)

  def +(that: Double): Complex =
    Complex(real + that, imag)

  def -(that: Complex): Complex =
    Complex(real - that.real, imag - that.imag)

  def -(that: Double): Complex =
    Complex(real - that, imag)

  def *(that: Complex): Complex =
    Complex(
      real * that.real - imag * that.imag,
      real * that.imag + imag * that.real
    )

  def *(that: Double): Complex =
    Complex(real * that, imag * that)

  def /(that: Complex): Complex =
    val d = that.real * that.real + that.imag * that.imag
    Complex(
      (real * that.real + imag * that.imag) / d,
      (imag * that.real - real * that.imag) / d
    )

  def /(that: Double): Complex =
    Complex(real / that, imag / that)

  def conj: Complex =
    Complex(real, -imag)

  override def equals(obj: Any): Boolean =
    obj match
      case that: Complex =>
        real == that.real && imag == that.imag
      case _ => false

  override def hashCode(): Int =
    (real, imag).##

  override def compare(that: Complex): Int =
    val r = this.real.compare(that.real)
    if r != 0 then r
    else this.imag.compare(that.imag)

end Complex

object PolarComplex:
  def apply(r: Double, theta: Double): Complex =
    Complex(
      r * math.cos(theta),
      r * math.sin(theta)
    )

extension (n: Int)
  def +(c: Complex): Complex =
    Complex(n.toDouble + c.real, c.imag)

  def *(c: Complex): Complex =
    Complex(n.toDouble * c.real, n.toDouble * c.imag)
