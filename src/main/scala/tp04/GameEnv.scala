package tp04

/**
 * Le type "GameEnv" modélise un environnement de jeu (le terrain, les cases
 *  de départ et d'arrivée, les concepts de position, bloc et mouvement).
 */
trait GameEnv {
  /** La classe "Pos" modélise une position sur le terrain de jeu.
   *  Rq : l'indice des lignes augmente quand on se déplace vers le bas.
   *       l'indice des colonnes augmente quand on se déplace vers la droite.
   */
  case class Pos(row: Int, col: Int) {
    /** La position obtenue en se décalant de "d" lignes. */
    def deltaRow(d: Int): Pos = copy(row = row + d)
    
    /** La position obtenue en se décalant de "d" colonnes. */
    def deltaCol(d: Int): Pos = copy(col = col + d)
  }

  /**
   * La position où se trouve le bloc au départ du jeu.
   * Cette valeur est abstraite. Elle sera fournie lors de la définition d'un
   *  jeu concret.
   */
  lazy val start: Pos

  /**
   * La cible à atteindre avec le bloc.
   * Cette valeur aussi est abstraite.
   */
  lazy val goal: Pos

  /**
   * Le type modélisant le terrain de jeu : une fonction booléenne qui vaut
   *  vrai ssi on lui fournit la position d'une case présente sur le terrain.
   */
  type Playground = Pos => Boolean

  /**
   * Le terrain de jeu.
   * Cette valeur aussi est abstraite.
   */
  lazy val playground: Playground

  /** Le type "Move" modélisant les mouvements possibles pour le bloc. */
  sealed abstract class Move
  case object Left  extends Move
  case object Right extends Move
  case object Up    extends Move
  case object Down  extends Move

  /**
   * Le bloc est caractérisé par la position de ses deux moitiés.
   * La première est toujours inférieure ou égale à la seconde dans l'ordre
   *  lexicographique (ligne - colonne).
   */
  case class Block(p1: Pos, p2: Pos) {

    /**
     * Cette méthode vérifie si le bloc est à la verticale. Elle le fait en comparant les colonnes des deux positions (p1.col et p2.col)
     * Si ces deux colonnes sont égales, cela signifie que les deux positions sont alignées verticalement, ce qui signifie que le bloc est en position verticale.
     * Dans ce cas, la méthode renvoie true sinon false
     */

    /** Le bloc est-il à la verticale ? */
    def isStanding: Boolean = p1.col == p2.col


    /**
     * Cette méthode vérifie si les deux moitiés du bloc sont sur le terrain de jeu. Elle le fait en appelant la fonction playground sur chaque position (p1 et p2).
     * Si les deux positions retournent true,
     * cela signifie qu'elles sont toutes deux sur le terrain de jeu, et donc le bloc est légal. La méthode renvoie true dans ce cas, sinon elle renvoie false.
     */

    /** Les deux moitiés du bloc sont-elles sur le terrain de jeu ? */
    def isLegal: Boolean = playground(p1) && playground(p2)


    /**
     * Cette méthode retourne un nouveau bloc obtenu en décalant les positions verticales de d1 lignes pour la première moitié (p1) et de d2 lignes pour la seconde moitié (p2).
     * Pour cela, elle appelle
     * la méthode deltaRow de la classe Pos sur chaque position pour effectuer le décalage vertical, puis crée un nouveau bloc avec les nouvelles positions obtenues.
     */
    /**
     * Retourne le bloc obtenu en décalant la première moitié de "d1" lignes
     *  et la seconde de "d2" lignes.
     */
    def deltaRow(d1: Int, d2: Int) = Block(p1.deltaRow(d1), p2.deltaRow(d2))


    /**
     * Cette méthode retourne un nouveau bloc obtenu en décalant les positions horiz de d1 lignes pour la première moitié (p1) et de d2 lignes pour la seconde moitié (p2).
     * Pour cela, elle appelle
     * la méthode deltaCol de la classe Pos sur chaque position pour effectuer le décalage horz, puis crée un nouveau bloc avec les nouvelles positions obtenues.
     */
    /**
     * Retourne le bloc obtenu en décalant la première moitié de "d1" colonnes
     *  et la seconde de "d2" colonnes.
     */
    def deltaCol(d1: Int, d2: Int) =  Block(p1.deltaCol(d1), p2.deltaCol(d2))


    /**
     * Cette méthode déplace le bloc vers la gauche en réduisant la colonne des deux positions de 1.
     * Cela est effectué en appelant la méthode deltaCol avec les valeurs -1 pour les deux décalages.
     */
    /** Le bloc obtenu en se déplaçant vers la gauche. */
    def left = deltaCol(-1, -1)


    /**
     * Cette méthode déplace le bloc vers la droite en réduisant la colonne des deux positions de 1.
     * Cela est effectué en appelant la méthode deltaCol avec les valeurs -1 pour les deux décalages.
     */
    /** Le bloc obtenu en se déplaçant vers la droite. */
    def right = deltaCol(1,1)


    /**
     * Cette méthode déplace le bloc vers le haut en réduisant la ligne des deux positions de 1.
     * Cela est effectué en appelant la méthode deltaRow avec les valeurs -1 pour les deux décalages.
     */
    /** Le bloc obtenu en se déplaçant vers le haut. */
    def up = deltaRow(-1,-1)


    /**
     * Cette méthode déplace le bloc vers le bas en réduisant la ligne des deux positions de 1.
     * Cela est effectué en appelant la méthode deltaRow avec les valeurs -1 pour les deux décalages.
     */
    /** Le bloc obtenu en se déplaçant vers le bas. */
    def down = deltaRow(1,1)


    /**
     * neighbours: Cette méthode retourne une liste de tuples, où chaque tuple contient un bloc voisin et le mouvement associé qui a permis
     * d'obtenir ce bloc. Pour ce faire, elle appelle les méthodes left, right, up et down pour obtenir les blocs déplacés dans chaque direction,
     * et les associe avec les mouvements correspondants Left, Right, Up et Down. La liste de tous ces tuples est retournée comme résultat.
     * */
    /**
     * Retourne la liste des blocs que l'on peut obtenir en un mouvement.
     * Chaque bloc est accompagné du mouvement qui a permis de l'obtenir.
     */
    def neighbours: List[(Block, Move)] = List(
      (left, Left),
      (right, Right),
      (up, Up),
      (down, Down)
    )

    /**
     * Cette méthode filtre la liste des voisins pour ne conserver que les blocs légaux. Pour cela, elle utilise la méthode filter sur
     * la liste des voisins, en vérifiant si le premier élément du tuple (le bloc) est légal en utilisant la méthode
     * isLegal définie précédemment. Seuls les blocs légaux sont conservés dans la
     * liste filtrée, tandis que les mouvements associés sont également conservés pour chaque bloc valide.
     */
    /**
     * Similaire à la méthode "neighbours", mais ne conserve que les blocs
     *  légaux.
     */
    def validNeighbours: List[(Block, Move)] =  neighbours.filter { case (block, _) => block.isLegal }
  }
}