import scala.io.Source

object Day3 {

  def deliverPresents(line: String): Set[(Int, Int)] = {
    def loop(idx: Int, pos: (Int, Int), visited: Set[(Int, Int)]):  Set[(Int, Int)] = {
      val visitedSoFar = visited + pos
      if (idx == line.length) {
        visitedSoFar
      } else {
        line(idx) match {
          case '>' => loop(idx + 1, (pos._1 + 1, pos._2), visitedSoFar)
          case '<' => loop(idx + 1, (pos._1 - 1, pos._2), visitedSoFar)
          case '^' => loop(idx + 1, (pos._1, pos._2 + 1), visitedSoFar)
          case 'v' => loop(idx + 1, (pos._1, pos._2 - 1), visitedSoFar)
          case nonDirection => throw new RuntimeException(s"Not a direction: $nonDirection")
        }
      }
    }
    loop(0, (0, 0), Set())
  }

  def deliverWithRobo(line: String): Set[(Int, Int)] = {
    val groupedInstructions = line.zipWithIndex.groupBy { t => t._2 % 2 == 0 }.mapValues(_.map(_._1))
    val visitedCoords = groupedInstructions.values.map(chars => deliverPresents(chars.mkString))
    visitedCoords.fold[Set[(Int, Int)]](Set())(_ ++ _)
  }

  def main(args: Array[String]): Unit = {
    val line = for (line <- Source.fromFile("problems/input3.txt").getLines().toList.head) yield line
    println(deliverPresents(line).size)
    println(deliverWithRobo(line).size)
  }
}
