import scala.collection.BitSet

object Day4 {
  val input = "yzbqklnj"

  def calculateMd5(message: String): String = {
    val s = Seq(
      7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,  7, 12, 17, 22,
      5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,  5,  9, 14, 20,
      4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,  4, 11, 16, 23,
      6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21,  6, 10, 15, 21
    )
    val k = (0 to 63).map { i =>
      ((1L << 32) * Math.abs(Math.sin(i + 1))).toLong
    }

    val a0 = 0x67452301L   //A
    val b0 = 0xefcdab89L   //B
    val c0 = 0x98badcfeL   //C
    val d0 = 0x10325476L   //D

    def padMessage(msg: String): String = {
      val lengthMod = 512 / 8
      val lengthGoal = 448 / 8
      if (msg.length % lengthMod == lengthGoal) {
        msg + (message.length * 8)
      } else {
        padMessage(msg + 0.toChar)
      }
    }
    val paddedMessage = padMessage(message + (1 << 8).toChar)
  }


  def main(args: Array[String]): Unit = {
    println(input)
  }

  /* https://en.wikipedia.org/wiki/MD5#Pseudocode */
}
