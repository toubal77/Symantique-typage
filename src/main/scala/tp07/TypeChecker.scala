package tp07

import scala.collection.immutable.ListMap
import TypeChecker._

class TypeChecker {
  /** Modélise un contexte de typage */
  private type Context = ListMap[Var, Typ]
  
  /** Retourne le type de "t" si "t" est typable, un message d'erreur sinon. */
  def checkType(t: Term): OptTyp = checkType(t, ListMap())

  private def checkType(t: Term, gamma: Context): OptTyp = t match {
    case Var(x) => gamma.get(x) match {
      case Some(typ) => SomeTyp(typ)
      case None => NoTyp(s"Variable $x non typée dans le contexte $gamma")
    }
    case Abs(x, typ, body) =>
      val newGamma = gamma + (x -> typ)
      checkType(body, newGamma) match {
        case SomeTyp(bodyType) => SomeTyp(Arrow(typ, bodyType))
        case NoTyp(msg) => NoTyp(msg)
      }
    case App(fun, arg) =>
      checkType(fun, gamma) match {
        case SomeTyp(Arrow(argType, resultType)) =>
          checkType(arg, gamma) match {
            case SomeTyp(argType2) if argType == argType2 => SomeTyp(resultType)
            case SomeTyp(argType2) => NoTyp(s"Type incorrect pour l'argument: $argType2, attendu: $argType")
            case NoTyp(msg) => NoTyp(msg)
          }
        case SomeTyp(nonArrowType) => Noyp(s"Type non-fonctionnel pour l'application: $nonArrowType")
        case NoTyp(msg) => NoTyp(msg)
      }
  }

}

object TypeChecker {
  /**
   * Modélise un type optionnel, avec un message d'erreur quand le typage
   *  n'a pu être réalisé.
   */
  sealed abstract class OptTyp
  case class SomeTyp(typ: Typ) extends OptTyp
  case class NoTyp(msg: String) extends OptTyp  
}