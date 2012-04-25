import LukeUtils._
import MachineLearningUtils._

object LDAWithCollapsedGibbsSampling {

  def inferTopics(documents: Vector[Vector[String]], numTopics: Int): Seq[Seq[String]] = {

    import scala.collection._

    val distinctWords = documents.flatten.distinct
    val distinctWordToIndex = distinctWords.zipWithIndex.toMap

    // M is the number of documents
    // K is the number of topics
    // y(a, b) is the b'th word in the a'th document
    // J is the number of distinct words

    val M = documents.length
    val K = numTopics
    val y = documents.zipWithIndex.flatMap {
      case (doc, docIdx) => doc.zipWithIndex.map {
        case (word, wordIdx) => ((docIdx, wordIdx), distinctWordToIndex(word))
      }
    }.toMap
    val J = distinctWords.length

    // alpha is the prior distribution over topics (whatever this means...)
    // beta is the prior distribution over words (whatever this means...)
    // gamma is the component value of alpha/beta since we're using flat priors
    val gamma = 0.1
    val alpha = Array.fill(K)(gamma)
    val beta = Array.fill(J)(gamma)

    // z(m, n) is the topic of the n'th word in the m'th document
    // c(k, m, j) is number of times word j is assigned to topic k in document m
    val z = mutable.Map.empty[(Int, Int), Int]
    val c = mutable.Map.empty[(Int, Int, Int), Int]

    // use a uniform random initial assignment of topic to each word z(_, _)
    y foreach { case (docAndWord, _) =>
        z(docAndWord) = scala.util.Random.nextInt(K)
    }

    // first fill c(_, _, _) with zeroes
    for (wordIdx <- 1 to J; docIdx <- 1 to M; topicIdx <- 1 to K)
      c((topicIdx - 1, docIdx - 1, wordIdx - 1)) = 0

    // update the word counts in c(_, _, _) according to our initial z(_, _):
    y foreach { case ((docIdx, docWordIdx), wordIdx) =>
      val wordTopic = z((docIdx, docWordIdx))
      c((wordTopic, docIdx, wordIdx)) += 1
    }

    // now for each doc-word (a, b), we calculate the most likely topic given all the
    // other topic assignments

    // Methods for summing over various indices
    def sumOverWords(f: Int => Double): Double = (0 to J - 1).map(f).sum
    def sumOverDocs(f: Int => Double): Double = (0 to M - 1).map(f).sum
    def sumOverTopics(f: Int => Double): Double = (0 to K - 1).map(f).sum
    def sumOverWordsAndDocs(f: (Int, Int) => Double): Double =
      sumOverWords{ w => sumOverDocs{ d => f(d, w) } }

    // calculate the denominator of the probability of some topic assignment z(a, b)
    // we can leave this out since it's independent of the test topic
    def calculateProbabilityDenominator(docIdx: Int, wordIdx: Int): Double = {
      sumOverTopics { k =>
        val cSumOverWords = sumOverWords { c(k, docIdx, _) }
        val alphaForTopic = alpha(k)

        val cSumOverDocs = sumOverDocs { c(k, _, wordIdx) }
        val betaForWord = beta(wordIdx)

        val cSumOverWordsAndDocs = sumOverWordsAndDocs { c(k, _, _) }

        (cSumOverWords + alphaForTopic) * (cSumOverDocs + betaForWord) /
        (cSumOverWordsAndDocs + gamma * J)
      }
    }

    // calculate the numerator of the probability of some topic assignment z(a, b)
    def calculateProbabilityNumerator(testTopic: Int, docIdx: Int, wordIdx: Int): Double = {
      val cSumOverWords = sumOverWords { c(testTopic, docIdx, _) }
      val alphaForOldTopic = alpha(testTopic)

      val cSumOverDocs = sumOverDocs { c(testTopic, _, wordIdx) }
      val betaForWord = beta(wordIdx)

      val cSumOverWordsAndDocs = sumOverWordsAndDocs { c(testTopic, _, _) }

      (cSumOverWords + alphaForOldTopic) * (cSumOverDocs + betaForWord) /
      (cSumOverWordsAndDocs + gamma * J)
    }

    for (assignment <- 1 to 1) {
      y foreach { case ((docIdx, docWordIdx), wordIdx) =>

        // first, we decrement the count in c to exclude the current doc-word
        val oldWordTopic = z((docIdx, docWordIdx))
        c((oldWordTopic, docIdx, wordIdx)) -= 1

        // now we determine the likelihood that the current doc-word has each topic
        // we're ignoring the denominator because we just want the most likely one
        // and don't need the probabilities to be normalized
        val topicLikelihoods = Array.fill(numTopics)(0.0)

        for (testTopic <- 0 to numTopics - 1) {
          val numerator = calculateProbabilityNumerator(testTopic, docIdx, wordIdx)
          topicLikelihoods(testTopic) = numerator
        }

        // find the most likely topic assignment for the current doc-word
        val newZ = topicLikelihoods.zipWithIndex.maxBy{ _._1 }._2

        z((docIdx, docWordIdx)) = newZ
        c((newZ, docIdx, wordIdx)) += 1
      }
    }

    // now, print out all the topics:

    val topics = z.toSeq map {
      case ((docIdx, docWordIdx), topicIdx) => (topicIdx, distinctWords(y(docIdx, docWordIdx)))
    } groupBy {
      x => x._1
    } mapValues {
      _ map { _._2 }
    }

    println(topics)

    topics.toSeq.map{ x => x._2 }
  }

  def extractDocumentsFromFile(text: String): Seq[String] = {
    import scala.xml._
    // Got to do some hackish stuff to escape the non-XML they store the AP data with...
    val escapedText = text.replace("&", "&amp;");
    val xmlDoc = XML.loadString("<?xml version=\"1.0\"?>\r\n<root>" + escapedText + "</root>")
    xmlDoc.child
      .map { node => (node\"TEXT").text }
      .filter { t => t != "" }
      .toList
  }

  def turnDocsIntoBagsOfWords(docs: Seq[String]): Vector[Vector[String]] =
    docs .map { doc => getWordSequenceFromString(doc).toVector } .toVector

  def main(args: Array[String]) {
    val numTopics = 5
    val numDocs = 20
    val fileText = readLocalTextFile("/Topics/ap.txt")
    println(fileText)
    val documents = extractDocumentsFromFile(fileText)
    println(documents)
    val bagsOfWords = turnDocsIntoBagsOfWords(documents)
    println(bagsOfWords take numDocs)
    // note: get rid of most common words before this
    val results = inferTopics(bagsOfWords take numDocs, numTopics)
    val betterResults = results map { topic => getCounts(topic).toSeq sortBy { -_._2 } drop 50 map { _._1 } }
    println(betterResults)
  }
}