package tp01.complex
import scala.math._

/**
 * Une classe modélisant les nombres complexes
 * On fera le nécessaire pour définir un ordre sur les complexes basé sur
 * l'équivalence (non conventionnelle) suivante :
 *  (a + bi < a' + b'i) <==> (a < a' || (a == a' && b < b'))
 * On permettra la création de complexes via leurs coordonnées polaires.
 * rappel : a + bi = (cos(argument) * module) + (sin(argument) * module) * i
 */
class Complex(val real: Double, val imag: Double) {
  /**
   * Pour afficher élégamment les nombres complexes, y compris quand la partie
   * réelle est nulle ou quand la partie imaginaire vaut -1, 0 ou 1
   */
  override def toString : String = {
    (real,imag) match {
      case (0, 0) => "0"
      case (0, 1) => "i"
      case (0, _) => s"${imag}i"
      case (_, 0) => s"$real"
      case (_, 1) => s"$real + i"
      case (_, -1) => s"$real - i"
      case (_, _) if imag < 0 => s"$real - ${-imag}i"
      case (_, _) => s"$real + ${imag}i"

    }
  }
  /**
   * Le module du nombre complexe
   * rappel : module(a + bi) = sqrt(a * a + b * b)
   */
  def mod = sqrt(real * real + imag * imag)
  /**
   * L'argument d'un nombre complexe
   * rappel : argument(c = a + bi) = acos(a / module(c))
   */
  def arg = acos(real / mod)
  /**
   * Le complexe obtenu en additionnant "this" et "that"
   */
  def +(that: Complex) = Complex(that.real+this.real, that.imag + this.imag)
  /**
   * Le complexe obtenu en soustrayant "that" à "this"
   */
  def -(that: Complex) = Complex(this.real -that.real, this.imag - that.imag)
  /**
   * Le complexe obtenu en multipliant "this" et "that"
   */
  def *(that: Complex) = Complex(this.real * that.real, this.imag * that.imag)
  /**
   * Le complexe obtenu en divisant "this" par "that"
   */
  def /(that: Complex) = Complex(this.real / that.real, this.imag / that.imag)
  /**
   * Fonction supérieur sur les nombre complexes
   */
  def >(that: Complex): Boolean = real > that.real || real == that.real && imag > that.imag
  /**
   * Fonction inférieur sur les nombre complexes
   */
  def <(that: Complex): Boolean = real < that.real || real == that.real && imag < that.imag
  /**
   * Le complexe conjugué de "this"
   * rappel : conj(a + bi) = a - bi
   */
  def conj = Complex(real, - imag)

  override def equals(that: Any): Boolean = {
    that match {
      case c: Complex => real == c.real && imag == c.imag
      case _ => false
    }
  }

  override def hashCode(): Int = (real + imag).toInt
}

object Complex {
  def apply(real: Double, imag: Double) = new Complex(real, imag)

  implicit def doubleToComplex(x:Double): Complex = new Complex(x, 0)
}

object PolarComplex {
  def apply(module: Double, argument: Double) = new Complex(cos(argument) * module, sin(argument) * module)
}