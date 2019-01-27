import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day12 extends App {

  private val initialState = "#....#.#....#....#######..##....###.##....##.#.#.##...##.##.#...#..###....#.#...##.###.##.###...#..#"

  val lines = Source.fromResource("day_12_input.txt").getLines().toList
  val answer1 = part1(lines)
  println(s"Part 1: $answer1")
  val answer2 = part2(lines)
  println(s"Part 2: $answer2")

  def part1(lines: List[String]): Int = {
    val indexOffset = 100
    val rules = lines.map(_.split(" => ")).map(arr => arr(0) -> arr(1).toCharArray()(0)).toMap
    var state = (("." * indexOffset) + initialState + ("." * indexOffset)).toCharArray
    val numGenerations = 20
    for (_ <- 0 until numGenerations) {
      val nextState = ArrayBuffer[Char]()
      for (i <- 2 until state.length - 2) {
        val window = state.slice(i - 2, i + 3)
        nextState append rules(window.mkString(""))
      }
      state = Array("..".toCharArray, nextState.toArray, "..".toCharArray).flatten
    }

    state
      .zip(Stream from -indexOffset)
      .filter { case (value, _) => value equals '#'}
      .map { case (_, index) => index }
      .sum

  }

  def part2(lines: List[String]): Int = {
    0
  }

}
