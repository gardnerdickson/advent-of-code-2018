
import scala.io.Source

object Day3 extends App {
  val lines = Source.fromResource("day_3_input.txt").getLines().toList
  val answer1 = part1(lines)
  println(s"Part 1: $answer1")
  val answer2 = part2(lines)
  println(s"Part 2: $answer2")

  def part1(lines: List[String]): Int = {
    val fabric = Array.fill(1000, 1000)(0)
    val claims = lines.map(Claim.parse)
    for (claim <- claims) {
      for (i <- claim.x until (claim.x + claim.width)) {
        for (j <- claim.y until (claim.y + claim.height)) {
          fabric(i)(j) += 1
        }
      }
    }
    fabric.flatten.count(x => x > 1)
  }

  def part2(lines: List[String]): Int = {
    val fabric = Array.fill(1000, 1000)(0)
    val claims = lines.map(Claim.parse)

    // number of squares that each claim consumes
    val claimSizes = claims.map(claim => claim.id -> claim.width * claim.height).toMap

    for (claim <- claims) {
      for (i <- claim.x until (claim.x + claim.width)) {
        for(j <- claim.y until (claim.y + claim.height)) {
          if (fabric(i)(j) == 0) fabric(i)(j) = claim.id else fabric(i)(j) = -1
        }
      }
    }

    val grouped = fabric.flatten.groupBy(x => x).map(entry => (entry._1, entry._2.length))
    val nonOverlappingClaims = grouped.filter {
      case (id: Int, squares: Int) => !Set(0, -1).contains(id) && (squares equals claimSizes(id))
    }
    if (nonOverlappingClaims.size == 1) {
      nonOverlappingClaims.toList.head._1
    } else {
      throw new RuntimeException(s"Expecting 1 non overlapping claim. Found ${nonOverlappingClaims.size}")
    }
  }

  object Claim {
    def parse(claim: String): Claim = {
      val tokens = claim.split("[@:]")
      if (tokens.length != 3) throw new IllegalArgumentException(s"$claim is not a valid claim.")
      val id = tokens(0).replace("#","").trim()
      val indexes = tokens(1).trim().split(",").map(_.toInt)
      val dimensions = tokens(2).trim().split("x").map(_.toInt)
      Claim(id.toInt, indexes(0), indexes(1), dimensions(0), dimensions(1))
    }
  }

  case class Claim(id: Int, x: Int, y: Int, width: Int, height: Int)

}


