package tp08

import Util._

class Evaluator {
  /** Réalise un pas d'évaluation, i.e. produit t' tel que t --> t'. */
  private def eval(t : Term) : Term = t match {
    // Règles de réduction pour les abstractions lambda
    case App(Abs(x, _, body), v) if isVal(v) => subst(x, v, body)

    // Règles de réduction pour les opérations sur les valeurs numériques
    case Succ(t1) if isNumVal(t1) => Succ(eval(t1))
    case Pred(Zero) => Zero
    case Pred(Succ(nv)) if isNumVal(nv) => nv
    case IsZero(Zero) => True
    case IsZero(Succ(nv)) if isNumVal(nv) => False

    // Règles de réduction pour les opérations conditionnelles
    case Cond(True, t2, _) => t2
    case Cond(False, _, t3) => t3
    case Cond(t1, t2, t3) => Cond(eval(t1), t2, t3)

    // Règles de réduction pour les applications
    case App(t1, t2) if !isVal(t1) => App(eval(t1), t2)
    case App(t1, t2) if isVal(t1) && !isVal(t2) => App(t1, eval(t2))
    case _ => t // Cas où aucun pas d'évaluation n'est possible
  }

  /** Evalue t jusqu'à obtenir un terme bloqué. */
  def evaluate(t : Term) : Term = {
    if (isVal(t)) t else evaluate(eval(t))
  }
}
