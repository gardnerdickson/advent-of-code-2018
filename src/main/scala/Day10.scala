import scala.io.Source

object Day10 extends App {

  val lines = Source.fromResource("day_10_input.txt").getLines().toList
  part1(lines)

  def part1(lines: List[String]): Unit = {
    val points = lines.map(Point(_)).toSet
    for (second <- 0 until 100000) {
      println(s" --- SECOND $second ---")
      points.foreach(_.move())
      val xMax = points.map(_.xPos).max
      val xMin = points.map(_.xPos).min
      val yMax = points.map(_.yPos).max
      val yMin = points.map(_.yPos).min
      val positions = points.map(point => (point.xPos, point.yPos))

      if (xMin + 100 >= xMax && yMin + 100 >= yMax) {
        for (row <- yMin - 10 until yMax + 10) {
          for (col <- xMin - 10 until xMax + 10) {
            if (positions.contains((col, row))) {
              print("#")
            } else {
              print(".")
            }
          }
          println
        }
      }
    }
  }

  object Point {
    private val pattern = "(-)*(\\d)+".r

    def apply(point: String): Point = {
      val matches = pattern.findAllIn(point).toList.map(_.toInt)
      Point(matches(0), matches(1), matches(2), matches(3))
    }
  }

  case class Point(var xPos: Int, var yPos: Int, xVel: Int, yVel: Int) {
    def move(): Unit = {
      this.xPos += this.xVel
      this.yPos += this.yVel
    }
  }
}
