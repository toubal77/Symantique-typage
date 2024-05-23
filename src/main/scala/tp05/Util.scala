package tp05

object Util {
  /** t est-il une valeur ? */
  def isVal(t: Term): Boolean = t match {
    case TRUE | FALSE | ZERO => true
    case _ => false
  }

  /** t est-il une valeur numérique ? */
  def isNumVal(t: Term): Boolean = t match {
    case ZERO => true
    case Succ(v) => isNumVal(v)
    case _ => false
  }
}
