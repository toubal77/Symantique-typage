package tp03

/**
 * Un objet permettant de coder et décoder des messages grâce à la méthode de
 *  Huffman.
 */
object Huffman {
  /**
   * Pour des raisons de simplicité (il s'agit d'un TP sur Scala, pas sur la
   *  compression), un bit sera représenté par un entier.
   * Ce n'est évidemment pas imaginable dans un contexte de compression réelle.
   */
  type Bit = Int

  /**
   * Un code de Huffman est un arbre binaire. 
   * Chaque feuille est associée à un caractère alphabétique et à son poids
   *  (généralement proportionnel à sa fréquence d'apparition dans le
   *  langage dans lequel sont écrits les messages).
   * Un noeud interne mémorise les caractères associés aux feuilles du
   *  sous-arbre dont il est la racine (dans l'ordre induit par un parcours en
   *  profondeur à main gauche préfixe de l'arbre) et la somme de leurs poids.
   */
  abstract class HuffmanTree
  case class Leaf(char: Char, weight: Int) extends HuffmanTree
  case class Node(left: HuffmanTree, right: HuffmanTree,
      chars: List[Char], weight: Int) extends HuffmanTree
  
  /**
   * Le poids d'un arbre est celui de sa racine.
   */
  def weight(tree: HuffmanTree): Int = tree match {
    case Leaf(_, w) => w
    case Node(_,_,_,w) => w
  }
  
  /**
   * La liste des caractères associés à la racine de "tree".
   */
  def chars(tree: HuffmanTree): List[Char] = tree match {
    case Leaf(c,_) => List(c) // comme c'est une leaf bah on retoun directement son caractere en List
    case Node(_,_,c,_) => c
  }
  
  /**
   * Renvoie un nouveau code de Huffman en faisant de "left" son sous-arbre
   *  gauche et de "right" son sous-arbre droit.
   */
  def buildTree(left: HuffmanTree, right: HuffmanTree): HuffmanTree =
    Node(left, right, chars(left) ::: chars(right) , weight(right) + weight(left)) // ::: pour concatener deux listes

  /**
   * Renvoie une liste de feuilles (codes de Huffman de taille 1), obtenue en
   *  calculant le nombre d'occurrences des caractères présents dans "chars".
   * Par exemple, si chars = (List('a', 'b', 'a')) le résultat sera :
   *  List(Leaf('a', 2), Leaf('b', 1)) ou List(Leaf('b', 1), Leaf('a', 2))
   * Il n'y a pas d'ordre imposé sur les éléments de la liste retournée.
   */
  def buildLeaves(chars: List[Char]): List[Leaf] = chars match {
    case Nil => List()
    case _ =>
      val charValue = chars.groupBy(identity).mapValues(_.size)  // map des caractères avec leurs occurrences
      charValue.map{case (char,size) => Leaf(char, size)}.toList // conversion en liste de feuilles
  }

  /**
   * On suppose que "trees" est ordonnée par poids croissants.
   * La fonction "insert" renvoie une liste équivalente à "trees"
   *  après y avoir inséré "t".
   * Cette fonction est pratique lorsqu'on réalise un tri (par
   *  insertion) d'une liste d'arbres.
   */
  def insert[T <: HuffmanTree](t: T, trees: List[T]): List[T] = {
    val (lower, upper) = trees.span(weight(_) < weight(t))
    lower ::: (t :: upper)
  }

  /**
   * Renvoie un code de Huffman à partir de "leaves".
   */
  def buildHuffmanTree(leaves: List[Leaf]): HuffmanTree = {
    def buildTreeAcc(trees: List[HuffmanTree]): HuffmanTree = trees match {
      case List(t) => t
      case t1 :: t2 :: ts => buildTreeAcc(insert(buildTree(t1, t2), ts))
      case _ => throw new IllegalArgumentException("Empty list of leaves")
    }

    buildTreeAcc(leaves)
  }

  /**
   * Décode la liste de bits "bits" avec le code "tree".
   */
  def decode(tree: HuffmanTree, bits: List[Bit]): List[Char] = {
    def decodeAcc(t: HuffmanTree, bits: List[Bit], decoded: List[Char]): List[Char] = (t, bits) match {
      case (Leaf(char, _), _) => decodeAcc(tree, bits, decoded :+ char)
      case (Node(left, right, _, _), b :: bs) => decodeAcc(if (b == 0) left else right, bs, decoded)
      case (_, Nil) => decoded
    }

    decodeAcc(tree, bits, Nil)
  }

  /**
   * Renvoie la liste de bits obtenue par encodage du texte "text" avec
   * le code de Huffman "tree".
   */
  def encode(tree: HuffmanTree, text: List[Char]): List[Bit] = {
    def encodeChar(tree: HuffmanTree, char: Char, bits: List[Bit]): List[Bit] = tree match {
      case Leaf(c, _) if c == char => bits
      case Node(left, right, _, _) =>
        if (chars(left).contains(char)) encodeChar(left, char, bits :+ 0)
        else encodeChar(right, char, bits :+ 1)
    }

    text.flatMap(char => encodeChar(tree, char, List.empty))
  }

  /**
   * Un outil plus efficace que l'arbre lui-même pour encoder un message :
   * une table de codage qui associe son code à chaque caractère.
   */
  type CodeTable = Map[Char, List[Bit]]

  /**
   * Convertit l'arbre en une table de codage équivalente.
   */
  def convert(tree: HuffmanTree): CodeTable = {
    def convertAcc(t: HuffmanTree, prefix: List[Bit], table: CodeTable): CodeTable = t match {
      case Leaf(char, _) => table + (char -> prefix.reverse)
      case Node(left, right, _, _) =>
        val leftTable = convertAcc(left, 0 :: prefix, table)
        convertAcc(right, 1 :: prefix, leftTable)
    }

    convertAcc(tree, List.empty, Map.empty)
  }

  /**
   * Renvoie la liste de bits obtenue par encodage du texte "text" avec
   * le code de Huffman "tree" (mais en utilisant en interne la table de
   * codage correspondante).
   */
  def fastEncode(tree: HuffmanTree, text: List[Char]): List[Bit] = {
    val table = convert(tree)
    text.flatMap(char => table(char))
  }
}