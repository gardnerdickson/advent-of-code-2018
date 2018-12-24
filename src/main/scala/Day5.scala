import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day5 extends App {
  val input = Source.fromResource("day_5_input.txt").mkString
  val answer1 = part1(input)
  println(s"Part 1: $answer1")
  val answer2 = part2(input)
  println(s"Part 2: $answer2")

  def part1(polymer: String): Int = {
    val polymerScanner = UnitWindow(ArrayBuffer(polymer.toCharArray: _*))
    react(polymerScanner)
  }

  def part2(polymer: String): Int = {
    val units = polymer.toCharArray.map(_.toLower).toSet
    val lengths = units.map(unit => {
      val remainingPolymer = polymer.replace(unit.toString, "").replace(unit.toUpper.toString, "").toCharArray
      val polymerScanner = UnitWindow(ArrayBuffer(remainingPolymer: _*))
      react(polymerScanner)
    })
    lengths.min
  }

  private def react(polymerScanner: UnitWindow): Int = {
    if (polymerScanner.isDone) {
      polymerScanner.characters.length
    } else {
      val first = polymerScanner.first()
      val second = polymerScanner.second()
      if ((first.toLower equals second.toLower) && (first.isLower && second.isUpper || first.isUpper && second.isLower)) {
        polymerScanner.removeWindow()
        polymerScanner.reset()
      } else {
        polymerScanner.increment()
      }
      react(polymerScanner)
    }
  }

  case class UnitWindow(characters: ArrayBuffer[Char]) {
    private var firstIndex = 0
    private var secondIndex = 1

    def isDone: Boolean = secondIndex >= characters.length

    def first(): Char = characters(firstIndex)

    def second(): Char = characters(secondIndex)

    def removeWindow(): Unit = {
      characters.remove(firstIndex)
      characters.remove(firstIndex)
    }

    def increment(): Unit = {
      firstIndex += 1
      secondIndex += 1
    }

    def reset(): Unit = {
      firstIndex = 0
      secondIndex = 1
    }

  }

}
