import Day15.{Elf, Goblin, Position, Space, Unit}

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

object Day15 extends App {

  val input = Source.fromResource("day_15_test.txt").getLines().toList
  val answer1 = part1(input)
  println(s"Part 1: $answer1")

  def part1(lines: List[String]): Int = {

    // Create game map
    val height = lines.size
    val width = lines.head.length
    val gameMap = ArrayBuffer.fill[Entity](width, height)(null)
    lines
      .map(_.zipWithIndex)
      .zipWithIndex
      .foreach { case (line, row) =>
        line.foreach { case (entity, column) =>
          gameMap(row)(column) = Entity(entity)
        }
      }


    def attackIfTargetInRange(position: Position, unit: Unit): Boolean = {
            // check if we are in range of a target
      val targetAndPositions = List[Position]((position.x, position.y - 1), (position.x + 1, position.y), (position.x, position.y + 1), (position.x - 1, position.y))
        .map(pos => (gameMap(pos.y)(pos.x), pos))
        .filter { case (entity, _) => unit.isTarget(entity) }
        .map { case (entity, pos) => (entity.asInstanceOf[Unit], pos) }

      if (targetAndPositions.nonEmpty) {
        val minHitPoints = targetAndPositions.map(_._1.hitPoints).min
        val filtered = targetAndPositions.filter { case (entity, _) => entity.hitPoints equals minHitPoints }
        val (target, position) = if (filtered.size == 1) {
          filtered.head
        } else {
          filtered.minBy(_._2)
        }

        target.hitPoints -= unit.attackPower
        if (target.hitPoints <= 0) {
          gameMap(position.y)(position.x) = Space()
          target match {
            case e: Elf => Elf.elves -= e
            case g: Goblin => Goblin.goblins -= g
          }
        }
        return true
      }
      false
    }


    var iterations = 0
    var endCombat = false
    while (!endCombat) {
      // iterate over game map and start executing turns
      for ((row, y) <- gameMap.zipWithIndex) {
        for ((unit, x) <- row.zipWithIndex.filter { case (entity, _) => entity.isUnit }.map { case (entity, x) => (entity.asInstanceOf[Unit], x) }) {
          val attacked = attackIfTargetInRange((x, y), unit)
          if (!attacked) { // otherwise, find the closest target and start moving towards it
            val nextPosition = determineNextPosition(gameMap, (x, y), unit)
            if (nextPosition.isDefined) {
              gameMap(y)(x) = Space()
              gameMap(nextPosition.get.y)(nextPosition.get.x) = unit
              attackIfTargetInRange((nextPosition.get.y, nextPosition.get.x), unit)
            }
          }

          val allMapUnits = gameMap.flatten.filter(_.isUnit)
          endCombat = unit match {
            case _: Elf => !allMapUnits.exists(_.isGoblin)
            case _: Goblin => !allMapUnits.exists(_.isElf)
            case _ => false
          }

        }
      }
      iterations += 1
      printMap(gameMap)
      println(s"iteration $iterations")
    }

    val hitPointsLeft = if (Elf.elves.nonEmpty) {
      Elf.elves.map(_.hitPoints).sum
    } else {
      Goblin.goblins.map(_.hitPoints).sum
    }

    println(s"iterations: $iterations")
    println(s"hit points left: $hitPointsLeft")
    iterations * hitPointsLeft
  }

  private def printMap(map: Seq[Seq[Entity]]): scala.Unit = {
    for (row <- map) {
      val rowEntities = ArrayBuffer[Unit]()
      for (entity <- row) {
        print(entity)
        entity match {
          case unit: Unit =>
            rowEntities.append(unit)
          case _ =>
        }
      }
      print("   ")
      rowEntities.foreach(entity => print(s"$entity(${entity.hitPoints}) "))
      println
    }
  }

  private def findUnit(gameMap: Seq[Seq[Entity]], target: Entity): Position = {
    for (row <- gameMap.indices) {
      for (col <- gameMap(row).indices) {
        if (gameMap(row)(col) equals target) {
          return Position(col, row)
        }
      }
    }
    throw new NoSuchElementException
  }

  private def determineNextPosition(gameMap: Seq[Seq[Entity]], startPosition: Position, startEntity: Unit): Option[Position] = {

    def getAdjacentPositions(position: Position): List[Position] = {
      List(
        (position.x, position.y - 1),
        (position.x + 1, position.y),
        (position.x, position.y + 1),
        (position.x - 1, position.y)
      )
    }

    val targetPositionBuffer = ArrayBuffer[Position]()
    for (row <- gameMap.indices) {
      for (col <- gameMap(row).indices) {
        gameMap(row)(col) match {
          case u: Unit =>
            if (u.isTarget(startEntity)) {
              val validTargetPositions = getAdjacentPositions(Position(col, row)).filter(pos => gameMap(pos.y)(pos.x).isSpace)
              targetPositionBuffer.appendAll(validTargetPositions)
            }
          case _ => // do nothing
        }
      }
    }


    def searchBfs(): List[Node] = {
      val paths = ArrayBuffer[Node]()
      val targetPositions = targetPositionBuffer.toSet
      val visited = mutable.Set[Position]()
      val queue = mutable.Queue(Node(startPosition, null))

      while (queue.nonEmpty) {
        val node = queue.dequeue()
        if (targetPositions.contains(node.position)) {
          paths.append(node)
        }
        val adjacentPositions = getAdjacentPositions(node.position).filter(pos => gameMap(pos.y)(pos.x).isSpace)//.map(pos => Node(pos, node))
        for (adjacentPosition <- adjacentPositions) {
          if (!visited.contains(adjacentPosition)) {
            visited.add(adjacentPosition)
            queue.enqueue(Node(adjacentPosition, node))
          }
        }
      }
      paths.toList
    }


    val paths = searchBfs()
    val nextPositions = paths.map(pathNode => {
      var node = pathNode
      while (node.parent.position != startPosition) {
        node = node.parent
      }
      node.position
    })
    nextPositions.sorted.headOption
  }


  object Entity {
    def apply(char: Char): Entity = {
      char match {
        case '#' => Wall()
        case '.' => Space()
        case 'E' => Elf()
        case 'G' => Goblin()
      }
    }
  }

  sealed abstract class Entity {
    def isWall: Boolean = false

    def isSpace: Boolean = false

    def isElf: Boolean = false

    def isGoblin: Boolean = false

    def isUnit: Boolean = false
  }

  object Wall {
    val walls: ArrayBuffer[Wall] = ArrayBuffer()
  }

  case class Wall() extends Entity {

    Wall.walls.append(this)

    override def isWall: Boolean = true

    override def toString: String = "#"
  }

  object Space {
    val spaces: ArrayBuffer[Space] = ArrayBuffer()
  }

  case class Space() extends Entity {

    Space.spaces.append(this)

    override def isSpace: Boolean = true

    override def toString: String = "."
  }

  sealed abstract class Unit(var hitPoints: Int = 200, var attackPower: Int = 3) extends Entity {

    def isTarget(unit: Entity): Boolean

    def hit(attackPower: Int)

    override def isUnit = true
  }

  object Goblin {
    private var id = 0

    protected def nextId(): Int = {
      id += 1
      id
    }

    val goblins: ArrayBuffer[Goblin] = ArrayBuffer()
  }

  case class Goblin(id: Int = Goblin.nextId()) extends Unit {

    Goblin.goblins.append(this)

    override def isGoblin: Boolean = true

    override def isTarget(unit: Entity): Boolean = unit.isInstanceOf[Elf]

    override def hit(attackPower: Int): scala.Unit = {
      this.hitPoints -= attackPower
      if (this.hitPoints <= 0) {
        Goblin.goblins -= this
      }
    }

    override def toString: String = "G"
  }

  object Elf {
    private var id = 0

    protected def nextId(): Int = {
      id += 1
      id
    }

    var elves: ArrayBuffer[Elf] = ArrayBuffer()
  }

  case class Elf(id: Int = Elf.nextId()) extends Unit {

    Elf.elves.append(this)

    override def isElf: Boolean = true

    override def isTarget(unit: Entity): Boolean = unit.isInstanceOf[Goblin]

    override def hit(attackPower: Int): scala.Unit = {
      this.hitPoints -= attackPower
      if (this.hitPoints <= 0) {
        Elf.elves -= this
      }
    }

    override def toString: String = "E"
  }


  object Position {
    implicit def tupleToPosition(tuple: (Int, Int)): Position = Position(tuple._1, tuple._2)
  }

  implicit def positionToTuple(position: Position): (Int, Int) = (position.x, position.y)

  case class Position(x: Int, y: Int) extends Ordered[Position] {
    override def toString = s"[$x,$y]"

    override def compare(that: Position): Int = {
      if (y < that.y) {
        -1
      } else if (y > that.y) {
        1
      } else {
        x.compareTo(that.x)
      }
    }
  }

  case class Node(position: Position, parent: Node)

}
