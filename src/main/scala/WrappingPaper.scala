import scala.io.Source

object WrappingPaper {
  def partOne(lines: Seq[String]): Int = {
    val paperPerPresent = lines.map { line =>
      val ints = line.split('x').map(Integer.parseInt)
      val sides = Seq(
        ints(0) * ints(1),
        ints(1) * ints(2),
        ints(2) * ints(0)
      )
      2 * sides.sum + sides.min
    }

    paperPerPresent.sum
  }

  def ribbon(lines: Seq[String]): Int = {
    val ribbonPerPresent = lines.map { line =>
      val sortedInts = line.split('x').map(Integer.parseInt).sorted
      val volume = sortedInts.fold(1)(_ * _)
      val aroundPresent = (sortedInts(0) + sortedInts(1)) * 2
//      println(s"Ribbon for $line was ${volume + aroundPresent}")
      volume + aroundPresent
    }
    ribbonPerPresent.sum
  }

  def main(args: Array[String]): Unit = {
    val lines = for (line <- Source.fromFile("problems/input2.txt").getLines().toList) yield line
    println(ribbon(lines))

  }
}
