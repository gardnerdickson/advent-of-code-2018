import scala.io.Source

object Day1 extends App {

  val lines = Source.fromResource("day_1_input.txt").getLines().toList
  val answer1 = part1(lines)
  println(s"Part 1: $answer1")
  val answer2 = part2(lines)
  println(s"Part 2: $answer2")

  def part1(lines: List[String]): Int = {
    lines.map(_.toInt).sum
  }

  def part2(lines: List[String]): Int = {
    var current = 0
    var seen = Set[Int]()
    Stream.continually(lines.map(_.toInt)).flatten.foreach(num => {
      current += num
      if (seen contains current) {
        return current
      } else {
        seen = seen + current
      }
    })
    throw new IllegalArgumentException("Should never reach here")
  }

}
