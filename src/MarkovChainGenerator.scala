import LukeUtils._

object MarkovChainGenerator {

  def generateSweetMarkovChainFromWarAndPeace() {
    // 1. build markov matrix from war and peace
    // 2. start with "the"
    // 3. generate 50 words after that

    val warAndPeace = readLocalTextFile("MarkovChain\\WarAndPeace.txt")
    val words = warAndPeace
      .filterNot { c => c == ',' || c == '.' || c == '"' }
      .map { _.toLower }
      .split ("\\s")
    val pairs = words zip (words drop 1)
    val props = pairs groupBy identity mapValues { _.length }

    val wordsToFollowingWordsAndCounts =
      props.toSeq
        .map { case ((a, b), num) => (a, (b, num)) }
        .groupBy { case (a, (b, num)) => a }
        .mapValues { _ map { case (a, (b, num)) => (b, num) } }

    val sortedByFrequency =
      wordsToFollowingWordsAndCounts mapValues { _ sortBy { _._2 } }

    def pickNext(word: String): String = {
      val possibleWordsAndWeights = sortedByFrequency(word)
      val rnd = scala.math.random
      val words = possibleWordsAndWeights map { _._1 }
      val weights = possibleWordsAndWeights map { _._2.toDouble }
      val summedWeights = weights.scan(0d) { _ + _ } drop 1
      val sumOfWeights = summedWeights.last
      val probs = summedWeights map { _ / sumOfWeights }
      words zip probs find { case (_, prob) => prob > rnd } map { _._1 } getOrElse "FAIL"
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