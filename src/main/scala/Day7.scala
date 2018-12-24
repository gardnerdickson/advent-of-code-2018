import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day7 extends App {

  val lines = Source.fromResource("day_7_input.txt").getLines().toList
  val answer1 = part1(lines)
  println(s"Part 1: $answer1")
  val answer2 = part2(lines)
  println(s"Part 2: $answer2")

  def part1(lines: List[String]): String = {
    val pattern = "Step ([A-Z]) must be finished before step ([A-Z]) can begin.".r
    val edges = lines.map { case pattern(first, second) => (first, second) }

    val terminalInstruction = (Set(edges.map(_._2): _*) diff Set(edges.map(_._1): _*)).head

    val adjacencyList = mutable.Map[String, mutable.Set[String]]()
    for (edge <- edges) {
      if (adjacencyList.contains(edge._1)) {
        adjacencyList(edge._1) += edge._2
      } else {
        adjacencyList(edge._1) = mutable.Set[String](edge._2)
      }
    }

    def run(instruction: String): String = {
      // Find all instructions that need to be completed before this instruction
      val beforeInstructions = adjacencyList.toMap.filter { case (_, list) => list.contains(instruction) }.keys
      ""
    }

    run(terminalInstruction)

    ""
  }

  def part2(lines: List[String]): String = {
    ""
  }

  case class Node(name: Char, children: ListBuffer[Char]) {

  }

}
