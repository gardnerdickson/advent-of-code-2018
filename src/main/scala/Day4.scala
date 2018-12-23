
import java.time.{LocalDateTime, ZoneId, ZoneOffset}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit

import Day4.Event.Event

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day4 extends App {

  val lines = Source.fromResource("day_4_input.txt").getLines().toList
  val answer1 = part1(lines)
  println(s"Part 1: $answer1")
  val answer2 = part2(lines)
  println(s"Part 2: $answer2")


  def part1(lines: List[String]): Int = {
    val records = lines.map(GuardRecord(_))
    val chronologicalRecords = records.sortBy(_.timestamp.toEpochSecond(ZoneOffset.UTC))
    val guardSleepMinutes = mutable.Map[Int, ListBuffer[LocalDateTime]]()

    var currentGuard = -1
    var sleepStart: LocalDateTime = null
    for (record <- chronologicalRecords) {
      record match {
        case GuardRecord(id, _, Event.BEGIN_SHIFT) =>
          currentGuard = id.get
        case GuardRecord(_, time, Event.FALL_ASLEEP) =>
          sleepStart = time
        case GuardRecord(_, time, Event.WAKE_UP) =>
          if (!guardSleepMinutes.contains(currentGuard)) {
            guardSleepMinutes(currentGuard) = ListBuffer()
          }
          val minutesAsleep = ChronoUnit.MINUTES.between(sleepStart, time).toInt

          for (i <- 0 until minutesAsleep) {
            guardSleepMinutes(currentGuard).append(sleepStart.plusMinutes(i))
          }
      }
    }

    val (guardId, sleepMinutes) = guardSleepMinutes.toList.maxBy { case (_, minutes) => minutes.size }
    val minuteMostAsleep = sleepMinutes.map(_.getMinute).groupBy(identity).mapValues(_.size).maxBy { case (_, count) => count }
    guardId * minuteMostAsleep._1
  }

  def part2(lines: List[String]): Int = {
    0
  }

  case class GuardRecord(id: Option[Int], timestamp: LocalDateTime, event: Event)

  object GuardRecord {
    private val wakeUpPattern = "\\[(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2})\\] wakes up".r
    private val fallAsleepPattern = "\\[(\\d{4}-\\d{2}-\\d{2}\\ \\d{2}:\\d{2})\\] falls asleep".r
    private val beginShiftPattern = "\\[(\\d{4}-\\d{2}-\\d{2}\\ \\d{2}:\\d{2})\\] Guard #([0-9]+) begins shift".r
    private val dateTimeFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

    def apply(record: String): GuardRecord = {
      record match {
        case wakeUpPattern(timestamp) =>
          GuardRecord(None, LocalDateTime.parse(timestamp, dateTimeFormat), Event.WAKE_UP)
        case fallAsleepPattern(timestamp) =>
          GuardRecord(None, LocalDateTime.parse(timestamp, dateTimeFormat), Event.FALL_ASLEEP)
        case beginShiftPattern(timestamp, guardId) =>
          GuardRecord(Some(guardId.toInt), LocalDateTime.parse(timestamp, dateTimeFormat), Event.BEGIN_SHIFT)
        case _ => throw new IllegalArgumentException(s"$record is not a valid record.")
      }
    }
  }

  object Event extends Enumeration {
    type Event = Value
    val WAKE_UP, FALL_ASLEEP, BEGIN_SHIFT = Value
  }

}
