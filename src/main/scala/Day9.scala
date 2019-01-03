import scala.collection.mutable.ListBuffer

object Day9 extends App {

  private val numPlayers = 439
  private val lastMarble = 71307

  val answer1 = run(numPlayers, lastMarble)
  println(s"Part 1: $answer1")
  val answer2 = run(numPlayers, lastMarble * 100)
  println(s"Part 1: $answer2")

  def run(numPlayers: Int, lastMarble: Int): Int = {
    val playerScores = Array.fill[Int](numPlayers)(0)
    var nextMarble = 0
    var nextMarblePosition = 0
    val circle = ListBuffer[Int]()
    while (nextMarble <= lastMarble) {
      if (nextMarble < 2) {
        circle.insert(nextMarble, nextMarble)
      } else if (nextMarble % 23 == 0) {
        val playerIndex = nextMarble % numPlayers
        playerScores.update(playerIndex, playerScores(playerIndex) + nextMarble)
        nextMarblePosition = if (nextMarblePosition - 7 < 0) {
          circle.length + (nextMarblePosition - 7)
        } else {
          nextMarblePosition - 7
        }
        playerScores.update(playerIndex, playerScores(playerIndex) + circle.remove(nextMarblePosition))
      } else {
        nextMarblePosition = if (nextMarblePosition + 2 > circle.length) {
          (nextMarblePosition + 2) % circle.length
        } else {
          nextMarblePosition + 2
        }
        circle.insert(nextMarblePosition, nextMarble)
      }
      nextMarble += 1
    }

    playerScores.max
  }

}
