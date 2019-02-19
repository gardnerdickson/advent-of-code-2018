import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day12 extends App {

  private val initialState = "#....#.#....#....#######..##....###.##....##.#.#.##...##.##.#...#..###....#.#...##.###.##.###...#..#"

  val lines = Source.fromResource("day_12_input.txt").getLines().toList
  val numGenerations = 20
  val answer = run(lines, numGenerations)
  println(s"Part 1: $answer")

  def run(lines: List[String], numGenerations: Int): Int = {
    val rules = lines.map(_.split(" => ")).map(arr => arr(0) -> arr(1).toCharArray()(0)).toMap
    var state = ("....." + initialState + ".....").toCharArray
    var startingIndex = -5
    var generation = 0
    var sum = 0
    var lastSum = 0
    while (generation < numGenerations) {
      val nextState = ArrayBuffer[Char]()
      for (i <- 2 until state.length - 2) {
        val window = state.slice(i - 2, i + 3)
        nextState append rules(window.mkString(""))
      }

      val firstPlant = nextState.indexOf('#')
      if (firstPlant < 5) {
        nextState.prepend("." * (5 - firstPlant):_*)
        startingIndex -= (5 - firstPlant)
      }
      val lastPlant = nextState.length - 1 - nextState.lastIndexOf('#')
      if (lastPlant < 5) {
        nextState.append("." * (5 - lastPlant):_*)
      }

      state = Array("..".toCharArray, nextState.toArray).flatten
      lastSum = sum
      sum = state
        .zip(Stream from startingIndex)
        .filter { case (value, _) => value equals '#'}
        .map { case (_, index) => index }
        .sum
      println(s"Generation: $generation, sum: $sum, sum difference: ${sum - lastSum}")
      generation += 1
    }

    sum
  }

}
