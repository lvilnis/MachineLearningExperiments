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
    val y = new Array[Array[Int]](M)
    documents.zipWithIndex flatMap {
      case (doc, docIdx) =>
        y(docIdx) = new Array[Int](doc.length)
        doc.zipWithIndex map {
          case (word, wordIdx) =>
            y(docIdx)(wordIdx) = distinctWordToIndex(word)
        }
    }
    val J = distinctWords.length

    // alpha is the prior distribution over topics (whatever this means...)
    // beta is the prior distribution over words (whatever this means...)
    // gamma is the component value of beta since we're using flat prior
    val gamma = 0.1
    val alpha = Array.fill(K)(50 / K)
    val beta = Array.fill(J)(gamma)

    // z(m, n) is the topic of the n'th word in the m'th document
    // c(k, m, j) is number of times word j is assigned to topic k in document m
    val z = new Array[Array[Int]](M)
    val c = Array.fill(K)(Array.fill(M)(Array.fill(J)(0)))

    // use a uniform random initial assignment of topic to each word z(_, _)
    for (doc <- 0 to M - 1) {
      val docLength = documents(doc).length
      z(doc) = new Array[Int](docLength)
      for (word <- 0 to docLength - 1)
        z(doc)(word) = scala.util.Random.nextInt(K)
    }

    // first fill c(_, _, _) with zeroes
    for (wordIdx <- 0 to J - 1; docIdx <- 0 to M - 1; topicIdx <- 0 to K - 1)
      c(topicIdx)(docIdx)(wordIdx) = 0

    // update the word counts in c(_, _, _) according to our initial z(_, _):
    for (doc <- 0 to M - 1) {
      val docLength = documents(doc).length
      for (word <- 0 to docLength - 1) {
        val wordTopic = z(doc)(word)
        val wordIdx = y(doc)(word)
        c(wordTopic)(doc)(wordIdx) += 1
      }
    }

    // now for each doc-word (a, b), we calculate the most likely topic given all the
    // other topic assignments

    // Methods for summing over various indices
    def sumOverWords(c: Array[Array[Array[Int]]], topic: Int, doc: Int): Int = {
      var sum, i = 0
      while (i < J - 1) { sum += c(topic)(doc)(i); i += 1 }
      sum
    }
    def sumOverDocs(c: Array[Array[Array[Int]]], topic: Int, word: Int): Int = {
      var sum, i = 0
      while (i < M - 1) { sum += c(topic)(i)(word); i += 1 }
      sum
    }
    def sumOverTopics(c: Array[Array[Array[Int]]], doc: Int, word: Int): Int = {
      var sum, i = 0
      while (i < K - 1) { sum += c(i)(doc)(word); i += 1 }
      sum
    }
    def sumOverWordsAndDocs(c: Array[Array[Array[Int]]], topic: Int): Int = {
      var sum, i, j = 0
      while ((i < M - 1) && (j < J - 1)) { sum += c(topic)(i)(j); i += 1; j += 1 }
      sum
    }

    // calculate the denominator of the probability of some topic assignment z(a, b)
    // we can leave this out since it's independent of the test topic
//    def calculateProbabilityDenominator(docIdx: Int, wordIdx: Int): Double = {
//      sumOverTopics { k =>
//        val cSumOverWords = sumOverWords { c(k, docIdx, _) }
//        val alphaForTopic = alpha(k)
//
//        val cSumOverDocs = sumOverDocs { c(k, _, wordIdx) }
//        val betaForWord = beta(wordIdx)
//
//        val cSumOverWordsAndDocs = sumOverWordsAndDocs { c(k, _, _) }
//
//        (cSumOverWords + alphaForTopic) * (cSumOverDocs + betaForWord) /
//        (cSumOverWordsAndDocs + gamma * J)
//      }
//    }

    // calculate the numerator of the probability of some topic assignment z(a, b)
    def calculateProbabilityNumerator(testTopic: Int, docIdx: Int, wordIdx: Int): Double = {
      val cSumOverWords = sumOverWords(c, testTopic, docIdx)
      val alphaForOldTopic = alpha(testTopic)

      val cSumOverDocs = sumOverDocs(c, testTopic, wordIdx)
      val betaForWord = beta(wordIdx)

      val cSumOverWordsAndDocs = sumOverWordsAndDocs(c, testTopic)

      (cSumOverWords + alphaForOldTopic) * (cSumOverDocs + betaForWord) /
      (cSumOverWordsAndDocs + gamma * J)
    }

    for (assignment <- 1 to 20) {
      for (docIdx <- 0 to (y.length - 1); docWordIdx <- 0 to (y(docIdx).length - 1)) {
        val wordIdx = y(docIdx)(docWordIdx)

        // first, we decrement the count in c to exclude the current doc-word
        val oldWordTopic = z(docIdx)(docWordIdx)
        c(oldWordTopic)(docIdx)(wordIdx) -= 1

        // now we determine the likelihood that the current doc-word has each topic
        // we're ignoring the denominator because we just want the most likely one
        // and don't need the probabilities to be normalized
        val topicLikelihoods = Array.fill(numTopics)(0.0)

        var i = 0
        while (i < numTopics - 1) {
          topicLikelihoods(i) = calculateProbabilityNumerator(i, docIdx, wordIdx)
          i += 1
        }

        println(topicLikelihoods.toList)

        // find the most likely topic assignment for the current doc-word
        // pick the next topic assignment according to the probabilities
        val newZ = MachineLearningUtils.getWeightedCasesDistribution(topicLikelihoods.zipWithIndex.map(x => (x._2, x._1)))()

        z(docIdx)(docWordIdx) = newZ
        c(newZ)(docIdx)(wordIdx) += 1
      }
    }

    // now, print out all the topics:

    val topics = z.zipWithIndex.toSeq flatMap { case (doc, docIdx) =>
      doc.zipWithIndex map { case (topic, wordIdx) =>
        (topic, distinctWords(y(docIdx)(wordIdx)))
      }
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
    val numTopics = 10
    val numDocs = 50
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