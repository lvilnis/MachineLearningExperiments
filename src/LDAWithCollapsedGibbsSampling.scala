import LukeUtils._
import MachineLearningUtils._
import scala.Predef._

object LDAWithCollapsedGibbsSampling {

  def inferTopics(documents: Array[Array[String]], numTopics: Int): Seq[Seq[String]] = {

    val distinctWords = documents.flatten.distinct
    val distinctWordToIndex = distinctWords.zipWithIndex.toMap

    // M is the number of documents
    // K is the number of topics
    // y(a, b) is the b'th word in the a'th document
    // J is the number of distinct words

    val M = documents.length
    val K = numTopics
    val J = distinctWords.length
    val y = Array.tabulate(M) { di =>
      val doc = documents(di)
      Array.tabulate(doc.length) { wi => distinctWordToIndex(doc(wi)) }
    }

    // alpha is the prior distribution over topics
    // beta is the prior distribution over words
    // gamma is the component value of beta since we're using flat priors
    val gamma = 0.1
    val alpha = Array.fill(K)(50 / K)
    val beta = Array.fill(J)(gamma)

    // z(a, b) is the topic of the b'th word in the a'th document
    // use a uniform random initial assignment of topic to each word
    val z = Array.tabulate(M) { di =>
      Array.fill(documents(di).length) { scala.util.Random.nextInt(K) }
    }

    // c(k, m, j) is number of times word j is assigned to topic k in document m
    val c = Array.fill(K, M, J)(0)

    // cj(k, m) is number of times any word is assigned topic k in document m
    // cm(k, j) is number of times word j is assigned topic k
    // cmj(k) is number of times any word is assigned topic k
    val cj = Array.fill(K, M)(0)
    val cm = Array.fill(K, J)(0)
    val cmj = Array.fill(K)(0)

    @inline def updateTopicCounts(topic: Int, doc: Int, word: Int, update: Int): Unit = {
      cj(topic)(doc) += update
      cm(topic)(word) += update
      cmj(topic) += update
      c(topic)(doc)(word) += update
    }

    // update the word counts in c according to our initial z:
    for (doc <- 0 until M; docWord <- 0 until z(doc).length) {
      val topic = z(doc)(docWord)
      val word = y(doc)(docWord)
      updateTopicCounts(topic, doc, word, 1)
    }

    // we'll reuse this array and overwrite it each go round
    val topicLikelihoods = Array.fill(numTopics)(0.0)

    for (assignment <- 1 to 1000;
         doc <- 0 until M;
         docWord <- 0 until z(doc).length) {

      val word = y(doc)(docWord)

      // first, we decrement the counts to exclude the current doc-word
      val oldTopic = z(doc)(docWord)
      updateTopicCounts(oldTopic, doc, word, -1)

      // now we determine the likelihood that the current doc-word has each topic
      // given all the other topic assignments
      var topic = 0
      while (topic < numTopics) {
        topicLikelihoods(topic) =
          (cj(topic)(doc) + alpha(topic)) *
          (cm(topic)(word) + beta(word)) /
          (cmj(topic) + gamma * J)
        topic += 1
      }

      // pick the next topic assignment according to the likelihoods
      val newTopic = pickTopic(topicLikelihoods)

      z(doc)(docWord) = newTopic
      updateTopicCounts(newTopic, doc, word, 1)
    }

    // now, group all the words into topics
    val topicWordPairs = z.zipWithIndex.toSeq flatMap { case (doc, d) =>
      doc.zipWithIndex map { case (t, w) => (t, distinctWords(y(d)(w))) }
    }
    val topics = topicWordPairs groupBy { _._1 } mapValues { _ map { _._2 } }
    topics.toSeq map { _._2 }
  }

  def pickTopic(topicLikelihoods: Array[Double]): Int = {
    val sumOfLikelihoods = fastSum(topicLikelihoods)
    val rouletteSpin = scala.util.Random.nextDouble() * sumOfLikelihoods
    var currentSlot = topicLikelihoods(0)
    var topic = 0
    while (currentSlot < rouletteSpin) {
      topic += 1
      currentSlot += topicLikelihoods(topic)
    }
    topic
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

  def turnDocsIntoBagsOfWords(docs: Seq[String], skipMostPopular: Int = 0): Array[Array[String]] = {
    val bags = docs map { getWordSequenceFromString(_) }
    val wordsAndCounts = getCounts(bags.flatten).toSeq
    val mostCommonWords = wordsAndCounts sortBy { -_._2 } take skipMostPopular map { _._1 }
    val commonWordSet = mostCommonWords.toSet
    val withoutCommonWords = bags map { _ filterNot commonWordSet }
    withoutCommonWords.map(_.toArray).toArray
  }

  def main(args: Array[String]) {
    timed("Inferred topics in %d ms" format _) {
      val numTopics = 40
      val numDocs = 240
      val skipMostCommonWords = 100
      val fileText = readLocalTextFile("/Topics/ap.txt")
      println(fileText)
      val documents = extractDocumentsFromFile(fileText) take numDocs
      println(documents)
      val bagsOfWords = turnDocsIntoBagsOfWords(documents, skipMostCommonWords)
      // note: get rid of most common words before this
      val results = inferTopics(bagsOfWords, numTopics)
      println(results map { getCounts(_).toSeq sortBy { -_._2 } map { _._1 } })
    }
  }
}