import LukeUtils._

object MarkovChainGenerator {

  def generateMarkovChainFromFile(fileName: String, startWord: String = "the"): Stream[String] = {

    val words = MachineLearningUtils.getWordSequenceFromLocalFile(fileName)
    val pairs = MachineLearningUtils.getConsecutivePairs(words)
    val pairsWithCounts = MachineLearningUtils.getCountsDouble(pairs)

    val wordsToFollowingWordsAndCounts =
      pairsWithCounts.toSeq
        .map({ case ((fst, snd), cnt) => (fst, (snd, cnt)) })
        .groupBy(_._1)
        .mapValues(_.map({ case (_, (snd, num)) => (snd, num) }))

    val sortedByFrequency = wordsToFollowingWordsAndCounts.mapValues(_.sortBy(_._2))

    var wordsToNextWordDists = Map(): Map[String, MachineLearningUtils.Dist[String]]

    def pickNext(word: String): String = {
      val possibleNextWordsAndWeights = sortedByFrequency(word)
      if (possibleNextWordsAndWeights.isEmpty) { return startWord }
      wordsToNextWordDists = wordsToNextWordDists.updated(
        word, MachineLearningUtils.getWeightedCasesDistribution(possibleNextWordsAndWeights))
      wordsToNextWordDists(word)()
    }

    Stream.iterate(startWord)(pickNext)
  }

  def main(args: Array[String]) {
    val filePath = "MarkovChain\\WarAndPeace.txt"
    val chain = generateMarkovChainFromFile(filePath)

    println(chain.take(100).reduce(_ + " " + _))
  }
}