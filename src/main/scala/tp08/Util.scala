package tp08

object Util {
  /**
   *  Construit une séquence d'applications de la forme t1 t2 ... tk
   * à partir de terms = List(t1, t2, ..., tk).
   * Rappel : l'application est associative à gauche
   *  (i.e. t1 t2 t3 ~ (t1 t2) t3)
   */
  def buildApp(terms: List[Term]): Term = terms match {
    case Nil => EOF
    case t :: Nil => t
    case t :: rest => App(t, buildApp(rest))
  }

  /**
   *  Construit un type fonctionnel de la forme typ1 -> typ2 -> ... -> typk
   * à partir d'une liste de types = List(typ1, typ2, ..., typk).
   * Rappel : le constructeur de type fonctionnel est associatif à droite
   *  (i.e. typ1 -> typ2 -> typ3 ~ typ1 -> (typ2 -> typ3))
   */
  def buildFctType(types: List[Typ]): Typ = types match {
    case Nil => throw new IllegalArgumentException("Liste de types vide")
    case t :: Nil => t
    case t :: rest => Arrow(t, buildFctType(rest))
  }

  /**  Remplace, dans t2, toutes les occurrences de x par t1. */
  def subst(x: Var, t1: Term, t2: Term): Term = t2 match {
    case Var(y) if x.name == y => t1
    case Abs(y, typ, body) if x.name != y.name => Abs(y, typ, subst(x, t1, body))
    case App(t1, t2) => App(subst(x, t1, t1), subst(x, t1, t2))
    case _ => t2
  }


  type Context = List[Val]

  /**
   *  Remplace, dans t, chaque alias du contexte ctx par le terme qui lui est
   *  associé.
   */
  def inject(ctx: Context, t: Term): Term = ctx.foldRight(t)((v, acc) => subst(v.x, v.t, acc))


  /** Si optT définit un nouvel alias, l'ajouter en tête du contexte ctx. */
  def buildNewCtx(ctx: Context, optT: Option[Term]): Context = optT match {
    case Some(t) => Val(Var("x"), t) :: ctx
    case None => ctx
  }

  /** t est-il une valeur ? */
  def isVal(t: Term): Boolean = t match {
    case Abs(_, _, _) => true
    case True | False | Zero => true
    case _ => false
  }

  /** t est-il une valeur numérique ? */
  def isNumVal(t: Term): Boolean = t match {
    case Zero => true
    case Succ(t1) => isNumVal(t1)
    case _ => false
  }

  /** t est-il un terme clos ? */
  def isClosed(t: Term): Boolean = {
    def freeVars(t: Term): Set[String] = t match {
      case Var(x) => Set(x)
      case Abs(x, _, body) => freeVars(body) - x
      case App(t1, t2) => freeVars(t1) ++ freeVars(t2)
      case _ => Set.empty
    }

    freeVars(t).isEmpty
  }
}