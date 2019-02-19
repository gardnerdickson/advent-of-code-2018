import Day13.Rail.Rail

import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day13 extends App {

  val lines = Source.fromResource("day_13_input.txt").getLines().toList
  val answer1 = part1(lines)
  println(s"Part 1: $answer1")
  val answer2 = part2(lines)
  println(s"Part 2: $answer2")

  def part1(lines: List[String]): Position = {
    val (track, cars) = parseInput(lines)
    var collisionPosition: Option[Position] = None
    while (collisionPosition.isEmpty) {
      for (car <- cars.sorted) {
        moveCar(car, track)
        val positions = cars.map(_.position)
        if (positions.length != positions.distinct.length) {
          collisionPosition = positions.diff(positions.distinct).distinct.headOption
        }
      }
    }
    collisionPosition.get
  }

  def part2(lines: List[String]): Position = {
    val (track, cars) = parseInput(lines)
    while (cars.filterNot(_.collided).size > 1) {
      for (car <- cars.sorted) {
        if (!car.collided) {
          moveCar(car, track)
          val collidingCars = cars.filterNot(_.collided).groupBy(_.position).filter { case (_, carsAtPos) => carsAtPos.length > 1 }.values.flatten
          for (collidingCar <- collidingCars) {
            collidingCar.collided = true
          }
        }
      }
    }
    cars.filterNot(_.collided).head.position
  }

  private def printTrack(track: Array[Array[Rail]], cars: Seq[Car]): Unit = {
    val carsByPosition = cars.groupBy(_.position)
    for (row <- track.indices) {
      println
      for (column <- track(row).indices) {
        val car = carsByPosition.get((column, row))
        if (car.isDefined) {
          print(car.get.head.direction.toString)
        } else {
          print(track(row)(column) match {
            case Rail.None => " "
            case Rail.Horizontal => "-"
            case Rail.Vertical => "|"
            case Rail.AscendingCorner => "/"
            case Rail.DescendingCorner => "\\"
            case Rail.Intersection => "+"
          })
        }
      }
    }
    println()
  }


  private def moveCar(car: Car, track: Array[Array[Rail]]): Unit = {

    // handle direction change
    val currentRail = track(car.position.y)(car.position.x)
    if (currentRail equals Rail.Intersection) {
      car.handleIntersection()
    } else if (currentRail equals Rail.DescendingCorner) {
      car.direction match {
        case Direction.LEFT => car.turn(Direction.UP)
        case Direction.UP => car.turn(Direction.LEFT)
        case Direction.RIGHT => car.turn(Direction.DOWN)
        case Direction.DOWN => car.turn(Direction.RIGHT)
      }
    } else if (currentRail equals Rail.AscendingCorner) {
      car.direction match {
        case Direction.RIGHT => car.turn(Direction.UP)
        case Direction.DOWN => car.turn(Direction.LEFT)
        case Direction.LEFT => car.turn(Direction.DOWN)
        case Direction.UP => car.turn(Direction.RIGHT)
      }
    }

    car.move()
  }

  def parseInput(lines: List[String]): (Array[Array[Rail]], Seq[Car]) = {
    val cars = ListBuffer[Car]()
    val track = lines.zipWithIndex.map {
      case (line, row) =>
        line.zipWithIndex.map {
          case (c, column) => c match {
            case ' ' => Rail.None
            case '-' => Rail.Horizontal
            case '|' => Rail.Vertical
            case '\\' => Rail.DescendingCorner
            case '/' => Rail.AscendingCorner
            case '+' => Rail.Intersection
            case '^' =>
              cars.append(Car((column, row), Direction.UP))
              Rail.Vertical
            case '>' =>
              cars.append(Car((column, row), Direction.RIGHT))
              Rail.Horizontal
            case 'v' =>
              cars.append(Car((column, row), Direction.DOWN))
              Rail.Vertical
            case '<' =>
              cars.append(Car((column, row), Direction.LEFT))
              Rail.Horizontal
            case _ => throw new IllegalArgumentException(s"File contains illegal character: $c")
          }
        }.toArray
    }.toArray
    (track, cars)
  }


  object Rail extends Enumeration {
    type Rail = Value
    val Vertical, Horizontal, DescendingCorner, AscendingCorner, Intersection, None = Value
  }


  case class Car(var position: Position, var direction: Direction, var collided: Boolean = false) extends Ordered[Car] {
    private var numTurns = 0L

    def handleIntersection(): Unit = {
      this.direction = numTurns % 3L match {
        case 0 => direction.left()
        case 1 => direction.straight()
        case 2 => direction.right()
      }
      numTurns += 1
    }

    def turn(direction: Direction): Unit = {
      this.direction = direction
    }

    def move(): Unit = {
      this.direction match {
        case Direction.UP => position = (position.x, position.y - 1)
        case Direction.DOWN => position = (position.x, position.y + 1)
        case Direction.LEFT => position = (position.x - 1, position.y)
        case Direction.RIGHT => position = (position.x + 1, position.y)
      }
    }

    override def compare(that: Car): Int = {
      if (this.position.y equals that.position.y) {
        this.position.x - that.position.x
      } else {
        this.position.y - that.position.y
      }
    }
  }

  object Position {
    implicit def tupleToPosition(x: (Int, Int)): Position = Position(x._1, x._2)
  }

  case class Position(x: Int, y: Int)

}