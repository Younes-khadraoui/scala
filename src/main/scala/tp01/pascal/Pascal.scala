package tp01.pascal

/**
 * Une petite application permettant d'afficher les premières lignes du triangle
 * de Pascal.
 */
object Pascal:

  def main(args: Array[String]): Unit =
    println("Le triangle de Pascal valeur par valeur :")
    printTriangle1(args(0).toInt)
    println("Le triangle de Pascal en un seul coup :")
    printTriangle2(args(0).toInt)

  /**
   * Renvoie la valeur de la case ("c", "r") du triangle de Pascal
   */
  def value(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else value(c - 1, r - 1) + value(c, r - 1)

  /**
   * Affiche les "n" premières lignes du triangle de Pascal valeur par valeur
   * (à l'aide de la méthode "value")
   */
  def printTriangle1(n: Int): Unit =
    for r <- 0 until n do
      for c <- 0 to r do
        print(value(c, r) + " ")
      println()

  type Line = List[Int]

  /**
   * Renvoie la ligne suivant "line" dans le triangle de Pascal
   */
  private def nextLine(line: Line): Line =
    val middle = line.sliding(2).collect {
      case List(a, b) => a + b
    }.toList
    1 :: middle ::: List(1)


  /**
   * Renvoie les "n" premières lignes du triangle de Pascal
   */
  def triangle(n: Int): List[Line] =
    if (n == 0) Nil
    else if (n == 1) List(List(1))
    else
      val tr = triangle(n - 1)
      tr :+ nextLine(tr.last)

  /**
   * Affiche les "n" premières lignes du triangle de Pascal ligne par ligne
   * (à l'aide de la méthode "triangle")
   */
  def printTriangle2(n: Int): Unit =
    triangle(n).foreach { line =>
      println(line.mkString(" "))
    }

end Pascal
