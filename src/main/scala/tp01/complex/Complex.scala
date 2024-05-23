package complex

import scala.math.sqrt
import scala.math.pow
import scala.math.cos

/**
 * Une classe modélisant les nombres complexes
 * On fera le nécessaire pour définir un ordre sur les complexes basé sur
 * l'équivalence suivante : 
 *  (a + bi < a' + b'i) <==> (a < a' || (a == a' && b < b'))
 * On permettra la création de complexes via leurs coordonnées polaires.
 * rappel : a + bi = (cos(argument) * module) + (sin(argument) * module) * i
 * 
 */
class Complex(val real: Double, val imag: Double) {
  /**
   * Pour afficher élégamment les nombres complexes, y compris quand la partie
   *  réelle est nulle ou quand la partie imaginaire vaut -1, 0 ou 1 
   */
  override def toString: Unit = print(real+" + "+imag+"i")
  /**
   * Le module du nombre complexe
   * rappel : module(a + bi) = sqrt(a * a + b * b)
   */
  def mod:Double = sqrt(pow(real,2)+pow(imag,2))
  /**
   * L'argument d'un nombre complexe
   * rappel : argument(c = a + bi) = cos^{-1}(a / module(c))
   */
  def arg:Double = 1 / cos(real/mod)
  /**
   * Le complexe obtenu en additionnant "this" et "that"
   */
  def +(that: Complex): Complex = new Complex(this.real+that.real,this.imag+that.imag)
  /**
   * Le complexe obtenu en soustrayant "that" à "this"
   */
  def -(that: Complex) = new Complex(this.real-that.real,this.imag-that.real)
  /**
   * Le complexe obtenu en multipliant "this" et "that"
   */
  def *(that: Complex) = new Complex(this.real*that.real-this.imag*that.imag,this.real*that.imag+that.real*this.imag)
  /**
   * Le complexe obtenu en divisant "this" par "that"
   */
  def /(that: Complex) ={
    val m = that.conj * this
    new Complex(m.real/(pow(that.real,2)-pow(that.imag,2)),m.imag/(pow(that.real,2)-pow(that.imag,2)))
  }
  /**
   * Le complexe conjugué de "this"
   * rappel : conj(a + bi) = a - bi
   */
  def conj = new Complex(this.imag,-this.imag)
}
