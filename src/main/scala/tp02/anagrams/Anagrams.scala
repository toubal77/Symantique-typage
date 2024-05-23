package tp02.anagrams

/**
 * Un objet fournissant des outils pour construire des anagrammes de mots
 *  et de phrases.
 *
 *
 *
 *
 *  moi:
 *
 *
 *  Objectif du Programme :
Le programme a pour objectif de générer des anagrammes pour une phrase donnée en utilisant un dictionnaire de mots.

Principes Généraux :

    Occurrences :
        La fréquence des caractères dans un mot ou une phrase est représentée par le type Occurrences, qui est une liste de tuples (Char, Int).

    Dictionnaire :
        Un dictionnaire de mots est fourni et utilisé pour rechercher des mots avec des occurrences spécifiques.

    Fonctions Principales :
        wordOccurrences: Convertit un mot en une liste d'occurrences.
        sentenceOccurrences: Convertit une phrase en une liste d'occurrences en combinant les occurrences de chaque mot.
        dictionaryByOccurrences: Groupe les mots du dictionnaire par leurs occurrences.
        wordAnagrams: Recherche les anagrammes d'un mot en utilisant le dictionnaire.
        combinations: Génère toutes les combinaisons d'occurrences possibles.
        subtract: Soustrait une liste d'occurrences d'une autre.
        sentenceAnagrams: Génère toutes les phrases anagrammes d'une phrase en utilisant la récursivité et les fonctions précédentes.

Fonctionnement du Programme :

    Occurrences des Mots :
        Les fonctions wordOccurrences et sentenceOccurrences convertissent les mots et les phrases en occurrences respectivement.

    Dictionnaire par Occurrences :
        La fonction dictionaryByOccurrences utilise le dictionnaire de mots pour regrouper les mots par leurs occurrences. Cela simplifie la recherche d'anagrammes.

    Recherche d'Anagrammes :
        La fonction wordAnagrams recherche les anagrammes d'un mot en utilisant le dictionnaire regroupé par occurrences.

    Génération des Anagrammes de Phrase :
        La fonction sentenceAnagrams est la fonction principale du programme. Elle utilise la récursivité et les fonctions auxiliaires
combinations et subtract pour générer toutes les combinaisons possibles d'anagrammes pour une phrase donnée.

Résumé du Processus :

    La phrase est convertie en occurrences.
    Les occurrences sont utilisées pour générer toutes les combinaisons possibles d'anagrammes de la phrase.
    Pour chaque combinaison, la fonction recherche des mots correspondants dans le dictionnaire.
    La récursivité est utilisée pour explorer toutes les options possibles.
    Le programme retourne finalement la liste complète des anagrammes de la phrase.

En résumé, le programme fonctionne en décomposant le problème d'anagramme en sous-problèmes plus simples, utilisant des structures de
données comme les occurrences et le dictionnaire pour rechercher et générer
des solutions. La récursivité est utilisée de manière cruciale pour explorer toutes les combinaisons possibles.
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

  private val dictionary: List[Word] = {
    val resourceFile = new java.io.File("src/main/scala/tp02/resources/dico.txt")
    val source = io.Source.fromFile(resourceFile)
    source.getLines.toList
  }

  /** 
   * Convertit un mot en la liste des fréquences de ses caractères.
   * Les éventuelles majuscules seront assimilées aux caractères minuscules
   *  correspondants.
   *
   *
   *  Moi:
   *  Cette méthode convertit un mot en une liste d'occurrences, c'est-à-dire une liste de
   *  fréquences de caractères. On commence par convertir le mot en minuscules pour
   *  simplifier les comparaisons. Ensuite, on utilise groupBy pour regrouper les caractères
   *  identiques, puis map pour compter la fréquence
   *  de chaque caractère. Enfin, on convertit le résultat en une liste triée.
   */
  def wordOccurrences(w: Word): Occurrences = w.toLowerCase().groupBy(char => char).map{ case (char, chars) => (char, chars.length)}.toList.sorted

  /** 
   * Convertit une phrase en la liste des fréquences de ses caractères.
   *
   *
   * Moi:
   * En résumé, la fonction sentenceOccurrences prend une liste de mots, convertit chaque mot en
   * une liste d'occurrences, groupe ces occurrences par caractère, calcule la somme des fréquences pour
   * chaque caractère, puis renvoie une liste triée d'occurrences représentant la fréquence
   * de chaque caractère dans la phrase entière.
   */
  def sentenceOccurrences(s: Sentence): Occurrences =
    s.flatMap(wordOccurrences).groupBy{ case (char, freq) => char}.mapValues(_.map(_._2).sum).toList.sorted

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
   * Cela revient à regrouper les mots du dictionnaire anagrammes les
   *  uns des autres.
   *
   *
   *
   *
   *
   *  Moi:
   *  Cette valeur paresseuse regroupe les mots du dictionnaire par leurs occurrences. On utilise la
   *  méthode groupBy sur le résultat de wordOccurrences appliquée à chaque mot du dictionnaire.
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy(wordOccurrences).withDefaultValue(List())

  /**
   * Renvoie la liste des anagrammes de "word".
   *
   *
   * Moi:
   * cette méthode retourne les anagrammes d'un mot en utilisant la valeur
   * dictionaryByOccurrences pour trouver les mots avec les mêmes occurrences.
   */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))
  /**
   * Retourne la liste de tous les "sous-ensembles" d'une liste de fréquences.
   * Par exemple : les sous-ensembles de la liste List(('a', 2), ('b', 2)) sont :
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
   *
   *
   *
   *
   *
   *
   *   moi:
   *
   * occurrences match { ... } :
   * La méthode utilise un motif de correspondance (match) sur la liste d'occurrences occurrences. Il y a deux cas possibles :
   * case Nil => List(List()) : Si la liste d'occurrences est vide, alors la combinaison possible est la liste vide elle-même, représentée par List(List()).
   * case (char, freq) :: tail => ... : Si la liste d'occurrences n'est pas vide et commence par un tuple (char, freq) suivi du reste tail, alors on poursuit avec les instructions dans la partie droite de =>.
   *
   * (for { i <- 0 to freq ... } yield ...).toList :
   * À l'intérieur du deuxième cas, une expression de compréhension de liste (for) est utilisée pour générer les combinaisons. Cela itère sur i de 0 à freq inclus.
   * Pour chaque valeur de i, la récursivité est utilisée pour générer les combinaisons possibles pour le reste de la liste (tail) à l'aide de combinations(tail).
   * La clause yield génère une liste d'occurrences pour chaque valeur de i. Si i est égal à 0, cela signifie que le caractère char n'est pas inclus dans la combinaison, donc on utilise rest (la combinaison pour le reste de la liste) telle quelle. Sinon, on ajoute le tuple (char, i) à la liste, représentant le fait que le caractère char est inclus i fois dans la combinaison.
   * La liste de toutes les combinaisons pour la valeur actuelle de i est obtenue à l'aide de yield.
   * Enfin, .toList est utilisé pour convertir toutes les combinaisons générées en une liste.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(List())
    case (char, freq) :: tail =>
      (for {
        i <- 0 to freq
        rest <- combinations(tail)
      } yield if (i == 0) rest else (char, i) :: rest).toList
  }

  /**
   * Renvoie la liste de fréquences obtenue en retirant "y" à "x".
   *
   *
   *
   *
   * val yMap = y.toMap.withDefaultValue(0) :
   * Cette ligne crée une map (yMap) à partir de la liste d'occurrences y. La clé de la map
   * est le caractère, et la valeur associée est la fréquence de ce caractère dans la liste d'occurrences
   * y. La méthode withDefaultValue(0) est utilisée pour spécifier que si une clé
   * n'est pas présente dans la map, sa valeur par défaut est 0.
   *
   * x.map { case (char, freq) => (char, freq - yMap(char)) } :
   * Cette partie utilise la méthode map sur la liste d'occurrences x. Pour chaque tuple (char, freq) dans
   * la liste x, elle crée un nouveau tuple où la fréquence est ajustée en soustrayant la fréquence correspondante
   * dans yMap. Cela représente la soustraction de la fréquence de chaque caractère dans y de sa fréquence dans x.
   *
   * .filter { case (_, freq) => freq > 0 } :
   * Ensuite, la méthode filter est utilisée pour ne conserver que les tuples où la fréquence résultante
   * est strictement supérieure à zéro. Cela élimine les caractères dont la fréquence après soustraction est nulle ou négative.
   *
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val yMap = y.toMap.withDefaultValue(0)
    x.map { case (char, freq) => (char, freq - yMap(char)) }.filter { case (_, freq) => freq > 0 }
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
   *
   *
   *
   *
   *
   *
   *
   * sentenceAnagramsHelper :
   * C'est une fonction auxiliaire récursive interne à sentenceAnagrams. Elle prend en paramètre une liste
   * d'occurrences (occurrences) représentant la fréquence des caractères dans la phrase.
   *
   * if (occurrences.isEmpty) List(List()) :
   * La condition de base de la récursivité. Si la liste d'occurrences est vide, cela signifie que tous les caractères
   * de la phrase ont été utilisés dans la construction de l'anagramme. Dans ce cas, la fonction retourne
   * une liste contenant une liste vide (List(List())). Cela représente un anagramme valide de la phrase vide.
   *
   * for { combination <- combinations(occurrences) ... } yield word :: rest :
   * La partie récursive de la fonction. Elle utilise une compréhension de liste pour générer toutes les combinaisons
   * possibles (combination) d'occurrences pour la phrase actuelle.
   * Pour chaque combinaison, elle sélectionne un mot (word) du dictionnaire (dictionaryByOccurrences) ayant ces occurrences.
   * Ensuite, elle réduit la liste d'occurrences en utilisant subtract, éliminant ainsi les caractères
   * utilisés pour le mot sélectionné.
   * La récursivité continue avec la liste réduite d'occurrences (rest).
   * La construction d'une liste d'anagrammes est réalisée avec yield word :: rest, où :: ajoute le mot actuel à la liste
   * d'anagrammes partielle.
   *
   * sentenceAnagramsHelper(sentenceOccurrences(sentence)) :
   * L'appel initial à sentenceAnagramsHelper. Il commence avec les occurrences initiales calculées à partir de la phrase (sentenceOccurrences(sentence)).
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def sentenceAnagramsHelper(occurrences: Occurrences): List[Sentence] =
      if (occurrences.isEmpty) List(List())
      else
        for {
          combination <- combinations(occurrences)
          word <- dictionaryByOccurrences(combination)
          rest <- sentenceAnagramsHelper(subtract(occurrences, combination))
        } yield word :: rest

    sentenceAnagramsHelper(sentenceOccurrences(sentence))
  }

}