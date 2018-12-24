
import scala.collection.mutable
import scala.io.Source

object Day6 extends App {
  val lines = Source.fromResource("day_6_input.txt").getLines().toList
  val answer1 = part1(lines)
  println(s"Part 1: $answer1")
  val answer2 = part2(lines)
  println(s"Part 2: $answer2")

  def part1(lines: List[String]): Int = {
    val coordinates = lines
      .map(line => line.split(","))
      .map(pair => (pair(0).trim().toInt, pair(1).trim.toInt))
      .zipWithIndex.map { case ((x, y), index) => index -> (x, y) }
      .toMap

    val (_, (maxX, _)) = coordinates.maxBy { case (_, (x, _)) => x }
    val (_, (_, maxY)) = coordinates.maxBy { case (_, (_, y)) => y }

    // For each position in the grid, find the given coordinate that it is closest to
    val grid = Array.fill[Int](maxX, maxY)(-1)
    for (x <- 0 until maxX) {
      for (y <- 0 until maxY) {
        val distances = coordinates
          .map { case (index, (xPos, yPos)) => index -> (Math.abs(xPos - x) + Math.abs(yPos - y)) }
          .toList
          .sortBy { case (_, distance) => distance }

        if (!distances(0)._2.equals(distances(1)._2)) {
          grid(x)(y) = distances.head._1
        }
      }
    }

    // Anything on an edge should be discounted
    val discounted = mutable.Set[Int]()
    for (i <- 0 until maxX) {
      if (grid(i)(0) != -1) discounted.add(grid(i)(0))
      if (grid(i)(maxY - 1) != -1) discounted.add(grid(i)(maxY - 1))
    }
    for (j <- 0 until maxY) {
      if (grid(0)(j) != -1) discounted.add(grid(0)(j))
      if (grid(maxX - 1)(j) != -1) discounted.add(grid(maxX - 1)(j))
    }

    val regionSizes = grid.flatten.filterNot(discounted.contains).groupBy(identity).mapValues(_.length)
    val (_, size) = regionSizes.maxBy { case (_, numSpaces) => numSpaces }
    size
  }

  def part2(lines: List[String]): Int = {
    val coordinates = lines
      .map(line => line.split(","))
      .map(pair => (pair(0).trim().toInt, pair(1).trim.toInt))
      .zipWithIndex.map { case ((x, y), index) => index -> (x, y) }
      .toMap

    val (_, (maxX, _)) = coordinates.maxBy { case (_, (x, _)) => x }
    val (_, (_, maxY)) = coordinates.maxBy { case (_, (_, y)) => y }

    var regionSize = 0
    for (x <- 0 until maxX) {
      for (y <- 0 until maxY) {
        val distances = coordinates.map { case (index, (xPos, yPos)) => Math.abs(xPos - x) + Math.abs(yPos - y) }.toList
        if (distances.sum < 10000) {
          regionSize += 1
        }
      }
    }
    regionSize
  }


}
