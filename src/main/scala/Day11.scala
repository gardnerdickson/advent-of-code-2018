object Day11 extends App {

  val input = 4151
  val answer1 = part1(input)
  println(s"Part 1: $answer1")
  val answer2 = part2(input)
  println(s"Part 2: $answer2")

  def part1(gridSerialNumber: Int): Coordinate = {
    val grid = Array.fill[Int](300, 300)(0)

    val rows = 300
    val cols = 300

    for (y <- 1 to rows) {
      for (x <- 1 to cols) {
        val rackId = x + 10
        var powerLevel = rackId * y
        powerLevel += gridSerialNumber
        powerLevel *= rackId
        powerLevel = ((powerLevel / 100) % 10) - 5
        grid(y - 1)(x - 1) = powerLevel
      }
    }

    var maxPowerLevel = Int.MinValue
    var maxPowerLevelCoordinate: Coordinate = (-1, -1)
    for (y <- grid.indices) {
      for (x <- grid(y).indices) {
        val squarePowerLevel = calculateSquarePower(grid, (x, y))
        if (squarePowerLevel > maxPowerLevel) {
          maxPowerLevel = squarePowerLevel
          maxPowerLevelCoordinate = (x + 1, y + 1)
        }
      }
    }

    maxPowerLevelCoordinate
  }


  case class Coordinate(x: Int, y: Int)

  implicit def tupleToCoordinate(tuple: (Int, Int)): Coordinate = Coordinate(tuple._1, tuple._2)

  private def calculateSquarePower(grid: Array[Array[Int]], startingCoordinate: Coordinate): Int = {
    if (startingCoordinate.x + 3 >= grid.length || startingCoordinate.y + 3 >= grid.length) Int.MinValue
    val subGrid = grid.slice(startingCoordinate.y, startingCoordinate.y + 3).map(row => row.slice(startingCoordinate.x, startingCoordinate.x + 3))
    subGrid.flatten.sum
  }

  def part2(gridSerialNumber: Int): Int = {
    0
  }

}
