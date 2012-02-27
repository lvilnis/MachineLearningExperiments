import scalala.library.Random
import LukeUtils._

object CleanedUpMarkov {

  def generateSweetMarkovChainFromWarAndPeace() {
    // Screw all this noise. Let's generate markov chains in the style of War and Peace!

    // 1. build markov matrix from war and peace
    // 2. start with "the"
    // 3. generate 50 words after that

    val warAndPeace = readLocalTextFile("WarAndPeace.txt")
    val words = warAndPeace
      .filterNot { c => c == ',' || c == '.' || c == '"' }
      .map { _.toLower }
      .split ("\\s")
    val pairs = words zip (words drop 1)
    val props = pairs groupBy identity mapValues { _.length }

    val nextThing =
      props.toSeq
        .map { case ((a, b), num) => (a, (b, num)) }
        .groupBy { case (a, (b, num)) => a }
        .mapValues { xs => xs map { case (a, (b, num)) => (b, num) } }

    // this is gonna take forever
    val thing2 = nextThing mapValues {xs => xs sortBy {_._2}}

    def pickNext(word: String): String = {
      val possibleWordsAndWeights = thing2(word)
      val rnd = Random.rand()(Random.mt)
      val words = possibleWordsAndWeights map {_._1}
      val weights = possibleWordsAndWeights map {_._2.toDouble}
      val summedWeights = (weights scan 0d) {_ + _} drop 1
      val sumOfWeights = summedWeights.last
      val probs = summedWeights map {_ / sumOfWeights}
      words zip probs find {case (nextWord, prob) => prob > rnd} map {_._1} getOrElse "FAIL"
    }

    val startWord = "the"

    def loop(word: String, i: Int, acc: String): String = {
      if (i > 50) acc
      else {
        val newWord = pickNext(word)
        loop(newWord, i + 1, acc + " " + newWord)
      }
    }

    println(loop(startWord, 0, startWord))
  }

    def main(args: Array[String]) {
      generateSweetMarkovChainFromWarAndPeace()
    }
}