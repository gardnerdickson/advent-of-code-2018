import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day7 extends App {

  private val numWorkers = 5

  val lines = Source.fromResource("day_7_input.txt").getLines().toList
  val answer1 = part1(lines)
  println(s"Part 1: $answer1")
  val answer2 = part2(lines)
  println(s"Part 2: $answer2")

  def part1(lines: List[String]): String = {
    val pattern = "Step ([A-Z]) must be finished before step ([A-Z]) can begin.".r
    val edges = lines.map { case pattern(first, second) => (first, second) }

    val (adjacencyLists, prerequisiteLists) = getStepMappings(edges)

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

  private def getStepMappings(edges: List[(String, String)]): (mutable.Map[String, mutable.Set[String]], mutable.Map[String, mutable.Set[String]]) = {
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
    (adjacencyLists, prerequisiteLists)
  }


  def part2(lines: List[String]): Int = {
    val pattern = "Step ([A-Z]) must be finished before step ([A-Z]) can begin.".r
    val edges = lines.map { case pattern(first, second) => (first, second) }

    val (adjacencyLists, prerequisiteLists) = getStepMappings(edges)

    val startSteps = (Set(edges.map(_._1): _*) diff Set(edges.map(_._2): _*)).toList.sorted

    val completed = mutable.Set[String]()
    var remainingSteps = ListBuffer[String](startSteps: _*)
    var runningSteps = ListBuffer[Step]()
    var totalTicks = 0
    while (remainingSteps.nonEmpty || runningSteps.nonEmpty) {
      var remainingStepIndex = 0
      while (remainingStepIndex < remainingSteps.length) {
        val step = remainingSteps(remainingStepIndex)
        if (!completed.contains(step) && !runningSteps.map(_.name).toSet[String].contains(step) && prerequisiteLists.getOrElse(step, completed).subsetOf(completed) && runningSteps.size < numWorkers) {
          runningSteps += Step(step)
          runningSteps = runningSteps.sorted
          remainingSteps.remove(remainingStepIndex)
          remainingStepIndex -= 1
        }
        remainingStepIndex += 1
      }

      var runningStepIndex = 0
      while (runningStepIndex < runningSteps.length) {
        val runningStep = runningSteps(runningStepIndex)
        runningStep.tick()
        if (runningStep.isDone) {
          runningSteps.remove(runningStepIndex)
          runningStepIndex -= 1
          completed += runningStep.name
          remainingSteps ++= adjacencyLists.getOrElse(runningStep.name, Set.empty[String])
          remainingSteps = ListBuffer(remainingSteps.toSet.toList.sorted: _*)
        }
        runningStepIndex += 1
      }

      totalTicks += 1
    }

    totalTicks
  }


  object Step {
    private val characterCodeOffset = 64
  }

  case class Step(name: String) extends Ordered[Step] {

    private var ticks: Int = 0
    private val totalDuration: Int = 60 + (name.toCharArray.head.toInt - Step.characterCodeOffset)
    private var done: Boolean = false

    def tick(): Int = {
      if (done) throw new IllegalStateException("")
      ticks += 1
      if (ticks - totalDuration == 0) {
        done = true
      }
      ticks
    }

    def isDone: Boolean = totalDuration - ticks == 0

    override def compare(that: Step): Int = this.name compare that.name

    override def toString: String = this.name
  }

}
