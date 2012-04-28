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

    // alpha is the prior distribution over topics (whatever this means...)
    // beta is the prior distribution over words (whatever this means...)
    // gamma is the component value of beta since we're using flat prior
    val gamma = 0.1
    val alpha = Array.fill(K)(50 / K)
    val beta = Array.fill(J)(gamma)

    // z(m, n) is the topic of the n'th word in the m'th document
    // c(k, m, j) is number of times word j is assigned to topic k in document m

    // use a uniform random initial assignment of topic to each word z(_, _)
    val z = Array.tabulate(M) { di =>
      Array.fill(documents(di).length) { scala.util.Random.nextInt(K) }
    }
    // first fill c(_, _, _) with zeroes
    val c = Array.fill(K, M, J)(0)

    val cOverWords = Array.fill(K, M)(0)
    val cOverDocs = Array.fill(K, J)(0)
    val cOverWordsAndDocs = Array.fill(K)(0)

    // update the word counts in c(_, _, _) according to our initial z(_, _):
    for (doc <- 0 until M; docWord <- 0 until z(doc).length) {
      val topic = z(doc)(docWord)
      val word = y(doc)(docWord)
      cOverWords(topic)(doc) += 1
      cOverDocs(topic)(word) += 1
      cOverWordsAndDocs(topic) += 1
      c(topic)(doc)(word) += 1
    }

    // we'll reuse this array and overwrite it each go round
    val topicLikelihoods = Array.fill(numTopics)(0.0)

    for (assignment <- 1 to 1000;
         doc <- 0 until M;
         docWord <- 0 until z(doc).length) {

      val word = y(doc)(docWord)

      // first, we decrement the count in c to exclude the current doc-word
      // we should pull these out to methods and see if they in-line... my guess is no because they're closures
      val oldTopic = z(doc)(docWord)
      c(oldTopic)(doc)(word) -= 1
      cOverWords(oldTopic)(doc) -= 1
      cOverDocs(oldTopic)(word) -= 1
      cOverWordsAndDocs(oldTopic) -= 1

      // now we determine the likelihood that the current doc-word has each topic
      // now for each doc-word (a, b), we calculate the most likely topic given all the
      // other topic assignments
      var topic = 0
      while (topic < numTopics) {
        topicLikelihoods(topic) =
          (cOverWords(topic)(doc) + alpha(topic)) *
          (cOverDocs(topic)(word) + beta(word)) /
          (cOverWordsAndDocs(topic) + gamma * J)
        topic += 1
      }

      //println(topicLikelihoods.toList)

      // find the most likely topic assignment for the current doc-word
      // pick the next topic assignment according to the probabilities
      val newTopic: Int = pickTopic(topicLikelihoods)

      z(doc)(docWord) = newTopic
      c(newTopic)(doc)(word) += 1
      cOverWords(newTopic)(doc) += 1
      cOverDocs(newTopic)(word) += 1
      cOverWordsAndDocs(newTopic) += 1
    }

    // now, print out all the topics:

    val topicWordPairs = z.zipWithIndex.toSeq flatMap { case (doc, d) =>
      doc.zipWithIndex map { case (t, w) => (t, distinctWords(y(d)(w))) }
    }

    val topics = topicWordPairs groupBy { _._1 } mapValues { _ map { _._2 } }

    println(topics)

    topics.toSeq map { _._2 }
  }

  def pickTopic(topicLikelihoods: Array[Double]): Int = {
    val sumOfLikelihoods = topicLikelihoods.sum
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
      val numDocs = 120
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