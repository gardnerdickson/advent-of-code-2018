import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

object Day2 extends App {

  val lines = Source.fromResource("day_2_input.txt").getLines().toList
  val answer1 = part1(lines)
  println(s"Part 1: $answer1")
  val answer2 = part2(lines)
  println(s"Part 2: $answer2")

  def part1(lines: List[String]): Int = {
    var twoLetters = 0
    var threeLetters = 0
    for (line <- lines) {
      val letters = line.toCharArray
      val letterCounts = mutable.Map(letters.toSet.map((letter: Char) => letter -> 0).toSeq: _*)
      letters.foreach(letter => {
        letterCounts(letter) = letterCounts(letter) + 1
      })

      var hasTwoLetters = false
      var hasThreeLetters = false
      for ((_, count) <- letterCounts) {
        if (count == 2) hasTwoLetters = true
        if (count == 3) hasThreeLetters = true
      }

      if (hasTwoLetters) twoLetters += 1
      if (hasThreeLetters) threeLetters += 1
    }

    twoLetters * threeLetters
  }

  def part2(lines: List[String]): String = {
    // Assume all lines are the same length
    for (i <- lines.indices) {
      for (j <- (i + 1) until lines.length) {
        val a = lines(i).toCharArray
        val b = lines(j).toCharArray

        val misses: ArrayBuffer[(Int, Char)] = ArrayBuffer()
        for (k <- a.indices) {
          if (!a(k).equals(b(k))) {
            misses append ((k, a(k)))
          }
        }
        if (misses.length == 1) {
          val buffer = ListBuffer(lines(i): _*)
          buffer.remove(misses(0)._1)
          return new String(buffer.toArray)
        }
      }
    }
    throw new IllegalArgumentException("Bad input")
  }

}
