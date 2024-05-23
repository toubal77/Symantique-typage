package pascal



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
  def main(args: Array[String]) {
    println("Le triangle de Pascal valeur par valeur :")
    printTriangle1(6)
    println("Le triangle de Pascal en un seul coup :")
    printTriangle2(6)
  }

  /**
    * Renvoie la valeur de la case ("c", "r") du triangle de Pascal
    */
  def value(c: Int, r: Int): Int = if(c == r || c == 0) 1 else value(c-1,r-1)+value(c,r-1)

  /**
    * Affiche les "n" premières lignes du triangle de Pascal valeur par valeur
    * (à l'aide de la méthode "value")
    */
  def printTriangle1(n: Int) = for(i <- 0 to n){
    for (j <- 0 to i) print(value(j , i)+" " )
    println(' ')
  }


  type Line = List[Int]

  /**
    * Renvoie la ligne suivant "line" dans le triangle de Pascal
    */
  def nextLine(line: Line): Line = line.size match {
    case 1 => List(line.head)
    case _ => nextLine(line.tail):::List(line.tail.head+line.head)
  }


  /* utiliser les match case */

  /**
    * Renvoie les "n" premières lignes du triangle de Pascal
    */
  def triangle(n: Int): IndexedSeq[Line] = if(n==0) IndexedSeq(List(1))
  else triangle(n-1):+(nextLine(triangle(n-1).last):::List(1))



  /**
    * Affiche les "n" premières lignes du triangle de Pascal ligne par ligne
    * (à l'aide de la méthode "triangle")
    */
  def printTriangle2(n: Int) = {
      val res = triangle(n)
      res.foreach(arg => {
        arg.foreach(el => print(el+" "))
        println
      })
  }

}