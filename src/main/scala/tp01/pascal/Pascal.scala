package tp01.pascal

/**
 * Une petite application permettant d'afficher les premières lignes du triangle
 *  de Pascal.
 * Deux méthodes sont proposées :
 * - l'une simple, mais gourmande en calculs, affiche les valeurs une par une au
 *  fil du calcul ;
 * - l'autre plus efficace en temps, mais plus technique et gourmande en mémoire,
 *  construit le triangle entièrement avant de l'afficher.
 */
object Pascal {
  def main(args: Array[String]): Unit = {
    println("Le triangle de Pascal valeur par valeur :")
    printTriangle1(args(0).toInt)
    println("Le triangle de Pascal en un seul coup :")
    printTriangle2(args(0).toInt)
  }

  /**
   * Renvoie la valeur de la case ("c", "r") du triangle de Pascal
   */
  def value(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else value(c - 1, r - 1) + value(c, r - 1)
  }

  /**
   * Affiche les "n" premières lignes du triangle de Pascal valeur par valeur
   * (à l'aide de la méthode "value")
   */
  def printTriangle1(n: Int) = {
    println(value(n, n) +" ")
  }

  type Line = List[Int]

  /**
   * Renvoie la ligne suivant "line" dans le triangle de Pascal
   */
  private def nextLine(line: Line): Line = ???

  /**
   * Renvoie les "n" premières lignes du triangle de Pascal
   */
  def triangle(n: Int): List[Line] = ???

  /**
   * Affiche les "n" premières lignes du triangle de Pascal ligne par ligne
   * (à l'aide de la méthode "triangle")
   */
  def printTriangle2(n: Int) = ???
}
