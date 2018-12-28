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

    // Map of each step to the set of steps that come next
    val adjacencyLists = mutable.Map[String, mutable.Set[String]]()
    // Map of each step to the set of prerequisite steps
    val prerequisiteLists = mutable.Map[String, mutable.Set[String]]()

    for ((step, nextStep) <- edges) {
      if (adjacencyLists.contains(step)) {
        adjacencyLists(step) += nextStep
      } else {
        adjacencyLists(step) = mutable.Set(nextStep)
      }
      if (prerequisiteLists.contains(nextStep)) {
        prerequisiteLists(nextStep) += step
      } else {
        prerequisiteLists(nextStep) = mutable.Set(step)
      }
    }

    // Start with the instructions that have no instruction preceding them
    val startSteps = (Set(edges.map(_._1): _*) diff Set(edges.map(_._2): _*)).toList.sorted

    val stepQueue = mutable.PriorityQueue[String](startSteps: _*)(implicitly[Ordering[String]].reverse)
    val stepBuffer = ListBuffer[String]()
    val orderedSteps = ListBuffer[String]()
    val completed = mutable.Set[String]()
    while (stepQueue.nonEmpty) {
      val step = stepQueue.dequeue()
      if (!completed.contains(step) && prerequisiteLists.getOrElse(step, completed).subsetOf(completed)) {
        orderedSteps.append(step)
        completed += step
        // The steps that could not be completed need to be put back on the queue
        stepQueue.enqueue(stepBuffer:_*)
        stepBuffer.clear()
        if (adjacencyLists.contains(step)) {
          stepQueue.enqueue(adjacencyLists(step).toSeq: _*)
        }
      } else {
        // Save the steps that can't be completed yet so they can be put back on the queue later
        stepBuffer.append(step)
      }
    }

    orderedSteps.mkString("")
  }

  def part2(lines: List[String]): String = {
    ""
  }

}
