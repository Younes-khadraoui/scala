package tp01.complex

/**
 * Une classe modélisant les nombres complexes
 */
class Complex(val real: Double, val imag: Double):
  /**
   * Pour afficher élégamment les nombres complexes, y compris quand la partie
   * réelle est nulle ou quand la partie imaginaire vaut -1, 0 ou 1
   */
  override def toString =
    if real == 0 && imag == 0 then "0"
    else if real == 0 then
      if imag == 1 then "i"
      else if imag == -1 then "-i"
      else s"${imag}i"
    else if imag == 0 then s"$real"
    else if imag == 1 then s"$real + i"
    else if imag == -1 then s"$real - i"
    else if imag > 0 then s"$real + ${imag}i"
    else s"$real - ${-imag}i"

  /**
   * Le module du nombre complexe
   * rappel : module(a + bi) = sqrt(a * a + b * b)
   */
  def mod = math.sqrt(real * real + imag * imag)

  /**
   * L'argument d'un nombre complexe
   * rappel : argument(c = a + bi) = acos(a / module(c))
   */
  def arg = math.acos(real / mod)

  /**
   * Le complexe obtenu en additionnant "this" et "that"
   */
  def +(that: Complex) = Complex(real + that.real, imag + that.imag)

  /**
   * Le complexe obtenu en additionnant "this" et "that"
   */
  def +(that: Double) = Complex(real + that, imag)

  /**
   * Le complexe obtenu en soustrayant "that" à "this"
   */
  def -(that: Complex) = Complex(real - that.real, imag - that.imag)

  /**
   * Le complexe obtenu en soustrayant "that" à "this"
   */
  def -(that: Double) = Complex(real - that, imag)

  /**
   * Le complexe obtenu en multipliant "this" et "that"
   */
  def *(that: Complex) = Complex(
    real * that.real - imag * that.imag,
    real * that.imag + imag * that.real
  )

  /**
   * Le complexe obtenu en multipliant "this" et "that"
   */
  def *(that: Double) = Complex(real * that, imag * that)

  /**
   * Le complexe obtenu en divisant "this" par "that"
   */
  def /(that: Complex) =
    val denom = that.real * that.real + that.imag * that.imag
    Complex(
      (real * that.real + imag * that.imag) / denom,
      (imag * that.real - real * that.imag) / denom
    )

  /**
   * Le complexe obtenu en divisant "this" par "that"
   */
  def /(that: Double) = Complex(real / that, imag / that)

  /**
   * Le complexe conjugué de "this"
   * rappel : conj(a + bi) = a - bi
   */
  def conj = Complex(real, -imag)

  /**
   * Comparaisons lexicographiques (réel puis imaginaire)
   */
  def >(that: Complex) = real > that.real || (real == that.real && imag > that.imag)
  def <(that: Complex) = real < that.real || (real == that.real && imag < that.imag)

  override def equals(that: Any) = that match
    case c: Complex => real == c.real && imag == c.imag
    case _          => false

  override def hashCode() = (real, imag).hashCode()
end Complex

object Complex:
  def apply(real: Double, imag: Double): Complex = new Complex(real, imag)

object PolarComplex:
  def apply(mod: Double, arg: Double): Complex =
    Complex(mod * math.cos(arg), mod * math.sin(arg))

extension (n: Int)
  def +(c: Complex): Complex = Complex(n.toDouble + c.real, c.imag)
