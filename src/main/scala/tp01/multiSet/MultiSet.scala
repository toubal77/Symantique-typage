package multiSet

/**
 * Une classe modélisant les multi-ensembles.
 * Un élément peut apparaitre plusieurs fois dans un multi-ensemble. On représentera
 *  donc ceux-ci à l'aide d'associations (Maps) dont les clés sont les éléments et
 *  les valeurs, leur nombre d'occurrences.
 * Par exemple, le multiset (e1, e2, e1, e3, e1, e3) sera représenté par
 *  l'association (e1 -> 3, e2 -> 1, e3 -> 2).
 */
class MultiSet[E](val elems: Map[E, Int]) {
  /**
   * Le nombre d'éléments 2 à 2 distinct dans le multiset, i.e. le nombre de clés
   *  de l'association.
   */
  def size: Int = elems.groupBy(identity).mapValues(_.size).size
  /**
   * Le nombre total d'éléments du multiset.
   */
  def card: Int = elems.size
  /**
   * "e" est-il présent dans le multiset ?
   */
  def mem(e: E):Boolean = elems.contains(e)
  /**
   * Le nombre d'occurrences de "e" dans le multiset.
   */
  def count(e: E):Int = elems filter(x => x._1 == e) size
  /**
   * "this" est-il sous-ensemble de "that" ?
   */
  def subsetOf(that: MultiSet[E]):Boolean = {
    for(x<- this.elems) yield if (!that.elems.contains(x._1)) false
    true
  }

  /**
   * Produit un nouveau multi-ensemble à partir de "this" auquel on ajoute "n"
   *  occurrences de l'élément "e".
   */
  def add(e: E, n: Int): MultiSet[E] = this.add(e,n)
  /**
   * Produit un nouveau multi-ensemble à partir de "this" dont on supprime "n"
   *  occurrences de l'élément "e".
   * Il ne reste aucune occurrence de "e" si "n" est supérieur ou égal à
   *  this.count(e) 
   */
  def remove(e: E, n: Int):MultiSet[E] = if(this.count(e) <= n)
    new MultiSet[E](this.elems - e)
  else new MultiSet[E](this.elems - e) add(e,n)
  /**
   * Produit un nouveau multi-ensemble union de "this" et "that".
   */
  def union(that: MultiSet[E]):MultiSet[E] = if (subsetOf(that)) that else{
    for((x,n) <- that.elems) this add(x,n)
    this
  }


  /**
   * Produit un nouveau multi-ensemble soustraction de "that" à "this".
   */
  def diff(that: MultiSet[E]):MultiSet[E] = {
    for((x,n) <- that.elems) this.remove(x,n)
    this
  }
  /**
   * Produit un nouveau multi-ensemble maximum de "this" et "that".
   * Le nombre d'occurrences d'un élément du maximum est le maximum des nombres
   *  d'occurrences de cet élément dans "this" et "that".
   */

  def maximum(that: MultiSet[E]) = { // comment supprimer la var dans cet algo
    var re : MultiSet[E] = this
    for ((x,n) <- that.elems){
      if(!re.mem(x)) this add(x,n)
      else re = this.remove(x,n).elems.get(x) match {
        case Some(y) => this add(x,y)
        case None => this
      }
    }
    re
  }
  /**
   * Produit un nouveau multi-ensemble intersection de "this" et "that".
   */
  def inter(that: MultiSet[E]):MultiSet[E] = this.union(that).diff(this.diff(that)).diff(that.diff(this))
  /**
   * L'égalité de multi-ensembles basée sur l'égalité ensembliste.
   */
  override def equals(that: Any):Boolean = that {
    case _: MultiSet[E] => diff(_).card != 0 false
    case _ => throw new Exception("Types différents")
  }
  /**
   * result == elems.hashCode()
   */
  override def hashCode() = elems.hashCode()
  /**
   * result == "MultiSet" + "(" + e1 + "->" + n1 + ", " + ... + ek + "->" + nk + ")"
   */
  override def toString():Unit = {
    print("(")
    for((x,n)<- elems) print(x+"->"+n)
    print(")")
  }

}
