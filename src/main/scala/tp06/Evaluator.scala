package tp06

import Util._

class Evaluator {
  /** Réalise un pas d'évaluation, i.e. produit t' tel que t --> t'. */
  // page 167
  // on match chaque term 
  def eval(t : Term) : Term = t match {
    // App(t1, t2) et t1 n'est pas un valeur on eval le t1
    case App(t1,t2) if !isVal(t1) => App(eval(t1),t2)
    // pariel avec t2
    case App(t1,t2) if !isVal(t2) => App(t1,eval(t2))
    // if t1 == Abs(x, t1) on applique la fonction du substition avec subst(x,t2,t1)
    case App(Abs(x,t1),t2) if isVal(t2) => subst(x,t2,t1)
    // if Val(v,t), on fait l'evaluation de t
    case Val(v,t) => Val(v,eval(t))
    // sinon c'est t
    case _ => t
  }
  /** Evalue t jusqu'à obtenir un terme bloqué. */
  def evaluate(t : Term) : Term = {
    if (isVal(t)) t else evaluate(eval(t))
  }
}
