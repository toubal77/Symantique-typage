package tp06

object Util {
  
  /** 
   * Construit une séquence d'applications de la forme t1 t2 ... tk
   *  à partir de terms = List(t1, t2, ..., tk).
   * Rappel : l'application est associative à gauche
   *  (i.e. t1 t2 t3 ~ (t1 t2) t3)
   */
  // on construit la séquence d'applications 
  // de gauche vers la droite de la liste avec reduceLeft def  [B>: A] (op: (B, A) ⇒ B): B
  def buildApp(terms : List[Term]) : Term = terms.reduceLeft((t1,t2) => App(t1,t2))

  /** Remplace, dans t2, toutes les occurrences de x par t1. */
  // page 185 des cours
  def subst(x : Var, t1 : Term, t2 : Term) : Term = t2 match {
    // t2 == var(y) and y == x donc t1
    case Var(y) if x.name == y => t1
    // t2 == app(tr1, tr2) on fait la rechareche dans les deux termes tr1 et tr2
    // donc appel récursive de subst
    case App(tr1, tr2) => App(subst(x, t1, tr1), subst(x, t1, tr2))
    // t2 == abs(y, t) on fait la recharche dans t 
    // donc appel recursive subst
    case Abs(v, t) if x != v => Abs(v, subst(x, t1, t))
    // priel que abs
    case Val(y, t) if x != y => Val(y, subst(x, t1, t))
    // sinon on retourne t2
    case _ => t2
  }
  
  /** Type des éléments d'un contexte. */
  type Alias = (Var, Term)
  
  /**
   *  Remplace dans t chaque alias du contexte ctx par le terme qui lui est
   *   associé.
   */
  // on commence par une valeur initiale (accumulateur) et à tous les éléments de la liste en partant de la gauche
  def inject(ctx : List[Alias], t : Term) : Term = ctx.foldLeft(t)((t, al) => subst(al._1, al._2, t))
  
  /** Si optT définit un nouvel alias, l'ajouter en tête du contexte ctx. */
  def buildNewCtx(ctx : List[Alias], optT : Option[Term]) : List[Alias] = optT match {
      // on vérife l'existance de alias grace à la méthode some si oui on l'ajoute en tête
      case Some(Val(x, t)) => (x,t)::ctx
      // sinon
      case _ => ctx
    }
  
  /** t est-il une valeur ? */
  def isVal(t : Term) : Boolean = t match {
    // verification de v, appel récursive de isVal avec v
    case Val(_, v) => isVal(v)
    // toujours vrai
    case Abs(_, _) => true
    // sinon false
    case _ => false
  }
  
  /** t est-il un terme clos ? */
  def isClosed(t : Term) : Boolean = {
    // methode auxilaire prend deux params t et une liste des var
    // retoyrne un bool
    def isClaux(t : Term, vars : List[Var]) : Boolean = {
      t match {
        // on verifie dans r
        case Abs(x, r) => isClaux(r, x::vars)
        // on verifie dans les deux termes tr1 et tr2
        case App(tr1, tr2) => isClaux(tr1, vars) && isClaux(tr2, vars)
        // on verifie la liste vars 
        case Var(x) if !vars.contains(Var(x)) =>  false
        // on verifie le terme t
        case Val(x, tr) => isClaux(tr, vars)
        // sinon
        case _ => true
      }
    }
    // initialisation de la isClaux
    isClaux(t, List[Var]())
  }
}