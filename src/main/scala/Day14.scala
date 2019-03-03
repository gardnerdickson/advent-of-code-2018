import scala.collection.mutable.{ArrayBuffer, ListBuffer}

object Day14 extends App {

  val input = 440231
  val answer1 = part1(input)
  println(s"Part 1: $answer1")
  val answer2 = part2(input.toString)
  println(s"Part 2: $answer2")

  def part1(numRecipes: Int): String = {
    val recipes = ArrayBuffer[Int](3, 7)
    var elfOneRecipeIndex = 0
    var elfTwoRecipeIndex = 1
    var counter = 0
    while (recipes.size <= numRecipes + 10) {
      // generate recipes
      val nextRecipe = recipes(elfOneRecipeIndex) + recipes(elfTwoRecipeIndex)
      if (nextRecipe > 9) {
        recipes.append(1, nextRecipe - 10)
      } else {
        recipes.append(nextRecipe)
      }

      // move current recipes
      elfOneRecipeIndex = calculateNextRecipeIndex(elfOneRecipeIndex, recipes)
      elfTwoRecipeIndex = calculateNextRecipeIndex(elfTwoRecipeIndex, recipes)

      if (counter > 100000) {
        println(recipes.size)
        counter = 0
      } else {
        counter += 1
      }
    }

    recipes.toList.slice(numRecipes, numRecipes + 10).mkString("")
  }

  def part2(sequence: String): Int = {
    val slice = sequence.map(Character.getNumericValue)
    val recipes = ArrayBuffer[Int](3, 7)
    var elfOneRecipeIndex = 0
    var elfTwoRecipeIndex = 1
    var counter = 0
    var indexOfSlice: Option[Int] = None
    while (indexOfSlice.isEmpty) {

      // generate recipes
      val nextRecipe = recipes(elfOneRecipeIndex) + recipes(elfTwoRecipeIndex)
      if (nextRecipe > 9) {
        recipes.append(1, nextRecipe - 10)
      } else {
        recipes.append(nextRecipe)
      }

      // move current recipes
      elfOneRecipeIndex = calculateNextRecipeIndex(elfOneRecipeIndex, recipes)
      elfTwoRecipeIndex = calculateNextRecipeIndex(elfTwoRecipeIndex, recipes)

      if (counter == 100000) {
        println(recipes.size)
        counter = 0
        if (recipes.containsSlice(slice)) {
          indexOfSlice = Some(recipes.indexOfSlice(slice))
        }
      } else {
        counter += 1
      }
    }
    recipes.indexOfSlice(slice)
  }

  private def calculateNextRecipeIndex(currentIndex: Int, recipes: Seq[Int]): Int = {
    var nextRecipeIndex = currentIndex + 1 + recipes(currentIndex)
    while (nextRecipeIndex >= recipes.size) {
      nextRecipeIndex = nextRecipeIndex - recipes.size
    }
    nextRecipeIndex
  }

}
