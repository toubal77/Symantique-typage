/**
 * Un objet fournissant des outils pour construire des anagrammes de mots
 *  et de phrases.
 */
object Anagrams {
  /**
   * Un mot est une String.
   * Pour simplifier, on supposera qu'un mot ne contient que des caractères
   *  alphabétiques ou des tirets.
   * Il n'y aura aucun caractère accentué ou porteur d'un tréma ou d'une cédille.
   */
  type Word = String
  type Sentence = List[Word]

  /** 
   * Une "Occurrences" est une liste ordonnée de couples (Char, Int) (selon
   *  l'ordre alphabétique, un caractère apparaissant au plus une fois dans la
   *  liste).
   * Elle permet d'associer à un mot ou une phrase, la liste de ses
   *  caractères avec leur fréquence dans le mot ou dans la phrase.

   * Remarque : si la fréquence d'un caractère est nulle alors il n'apparait
   *  pas dans la liste.
   */
  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = {
    val resourceFile = new java.io.File("src/resources/dico.txt")
    val source = io.Source.fromFile(resourceFile)
    source.getLines.toList
  }

  /** 
   * Convertit un mot en la liste des fréquences de ses caractères.
   * Les éventuelles majuscules seront assimilées au caractère minuscule
   *  correspondant.
   */
  /*  variante sans toLowerCase
  def wordOccurrences(w: Word): Occurrences = w.size match {
    case 1 => List[(Char, Int)]((w(0),w.count(_ == w(0))))
    case _ => List[(Char, Int)]((w(0), w.count(_ == w(0)))):::wordOccurrences(w.replace(w(0)+"", ""))
  }*/
  def wordOccurrences(w: Word): Occurrences =
    w.toLowerCase().toList.groupBy(c => c).mapValues(_.size).toList.sorted


  /** 
   * Convertit une phrase en la liste des fréquences de ses caractères
   */
  /*def sentenceOccurrences(s: Sentence): Occurrences = s.size match {
    case 1 => wordOccurrences(s.head)
    case _ => sentenceOccurrences(s.tail):::wordOccurrences(s.head)
  }*/

  /* variante 1
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.toStream.mkString)
   */
  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s reduceRight (_ + _))



  /** 
   * Une association qui fait correspondre à une liste de
   *  fréquences de caractères, les mots du dictionnaires
   *  compatibles avec cette liste.
   * Par exemple, les occurrences de caractères du mot "tri" sont :
   *  List(('i', 1), ('r', 1), ('t', 1))
   * Ce sont les mêmes occurrences pour les mots "tir" et "rit".
   * On aura donc l'entrée suivante dans l'association
   *  "dictionaryByOccurrences" :
   *  List(('i', 1), ('r', 1), ('t', 1)) -> List("rit", "tir", "tri")
   *
   */
  val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    (dictionary map (word => (wordOccurrences(word), word))) groupBy (n => n._1) mapValues (x => x map (y => y._2)) withDefaultValue List()

  /**
   * Renvoie l'anagramme de "word"
   */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  /**
   * Retourne la liste de tous les sous-ensembles d'une liste de fréquences.
   * Par exemple: les sous-ensembles de la liste List(('a', 2), ('b', 2)) sont :
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = {
    if (occurrences.isEmpty) List(List())
    else {
      val head = occurrences.head
      val combos: List[Occurrences] = combinations(occurrences.tail)
      (for {
        combo <- combos
        n <- 1 to head._2
      } yield (head._1, n) :: combo) ++ combos
    }
  }
  
  /**
   * Renvoie la liste de fréquences obtenue en retirant "y" à "x".
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    y.foldLeft(x.toMap)((occs, occ) =>
      if (occ._2 == occs(occ._1)) occs - occ._1
      else occs updated (occ._1, occs(occ._1) - occ._2)
    ).toList
  }
  
  /**
   * Renvoie la liste de toutes les phrases anagrammes de "sentence".
   * Par exemple, pour le paramètre List("a", "plus"), la méthode renvoie :
   *    List(
   *      List("su", "pal")
   *      List("us", "pal")
   *      List("pu", "las")
   *      List("lu", "pas")
   *      List("plus", "a")
   *      List("a", "plus")
   *      List("pas", "lu")
   *      List("las", "pu")
   *      List("pal", "su")
   *      List("pal", "us")
   *    )
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    if (sentence.isEmpty) List(List())
    else {
      def occurrenceAnagrams(occurrences: Occurrences): List[Sentence] =
        if (occurrences.isEmpty) List(List())
        else {
          for {
            occurrence <- combinations(occurrences)
            word <- dictionaryByOccurrences(occurrence)
            sentence <- occurrenceAnagrams(subtract(occurrences, occurrence))
          } yield word :: sentence
        }
      occurrenceAnagrams(sentenceOccurrences(sentence))
    }
  }

}