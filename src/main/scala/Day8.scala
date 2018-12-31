import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day8 extends App {

  val numbers = Source.fromResource("day_8_input.txt").mkString.split(" ").map(_.toInt)
  val answer1 = part1(numbers)
  println(s"Part 1: $answer1")
  val answer2 = part2(numbers)
  println(s"Part 2: $answer2")

  def part1(numbers: Array[Int]): Int = {
    val root = createNode(numbers)
    val metadata = flattenMetadata(root)
    metadata.sum
  }

  private def createNode(numbers: Array[Int], index: Index = Index(0)): Node = {
    val header = Header(numbers(index.value), numbers(index.increment().value))
    val children = ListBuffer[Node]()
    for (_ <- 0 until header.childQuantity) {
      if (index.value + 1 < numbers.length) {
        children.append(createNode(numbers, index.increment()))
      }
    }
    val metadata = ListBuffer[Int]()
    for (_ <- 0 until header.metadataQuantity) {
      metadata.append(numbers(index.increment().value))
    }

    Node(header, children.toList, metadata.toList)
  }

  private def flattenMetadata(node: Node): List[Int] = {
    val metadata = node.metadata
    metadata ++ node.children.flatMap(flattenMetadata)
  }

  def part2(numbers: Array[Int]): Int = {
    val root = createNode(numbers)
    resolveNodeValue(root)
  }

  private def resolveNodeValue(node: Node): Int = {
    if (node.children.isEmpty) {
      node.metadata.sum
    } else {
      val referencedChildren = node.metadata.filter(1 to node.children.length contains _).map(x => node.children(x - 1))
      referencedChildren.map(resolveNodeValue).sum
    }
  }

  case class Header(childQuantity: Int, metadataQuantity: Int)
  case class Node(header: Header, children: List[Node], metadata: List[Int])

  case class Index(var value: Int) {
    def increment(): Index = {
      value += 1
      this
    }
  }

}
