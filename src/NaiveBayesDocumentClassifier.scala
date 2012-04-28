
import LukeUtils._
import MachineLearningUtils._

object NaiveBayesDocumentClassifier {

  abstract sealed class BritishAuthor
  case object OscarWilde extends BritishAuthor
  case object CharlesDickens extends BritishAuthor

  type BritClassifier = DocumentClassifier[BritishAuthor]

  // note to self: use "Dispatch" library to download from Project Gutenberg directly....

  val trainingTextFileNamesAndClassifications: Set[(String, BritishAuthor)] =
    Set((      "GreatExpectations", CharlesDickens),
        ( "TaleOfTwoCities", CharlesDickens),
        ("TheHappyPrinceAndOtherTales", OscarWilde),
        ("ThePictureOfDorianGray", OscarWilde))

  val textFileNamesToClassify =
    Set(
      "AChristmasCarol",
        "OliverTwist",

        "ImportanceOfBeingErnest",
        "TheCantervilleGhost")

  def getClassifierFromTrainingData(trainingSet: Set[(String, BritishAuthor)]): String => BritishAuthor = {

    val authorsAndWordSequences = trainingSet map { case (a, b) => (b, getWordSequenceFromLocalFile("Classification\\" + a + ".txt")) }

    val authorsToAllWords = groupPairsByFirst(authorsAndWordSequences.toSeq) mapValues { _ flatten }
    val authorsToWordsAndCounts = authorsToAllWords mapValues { getCounts _ }
    val authorsToTotalWords = authorsToAllWords mapValues { _.length }
    val vocabulary = authorsToWordsAndCounts.values.map{ _.keys }.flatten.toSet

    // now we want to calculate, foreach word and author,
    // P(word | author) = (numOccurencesForAuthor(word, author) + 1) / (totalNumWords(author) + numDistinctWordsInAllTrainingData)
    def logLikelihoodOfSeeingWordGivenAuthor(word: String, author: BritishAuthor): Double =
      math.log((authorsToWordsAndCounts(author) get word getOrElse 0) + 1) - math.log(authorsToTotalWords(author) + vocabulary.size)

    // we should make it throw out the super common words...

    // P(author) - pct of training samples that were this author
    val authorsToPctTrainingSamples = trainingSet groupBy { _._2 } mapValues {
      _.size.asInstanceOf[Double] / trainingSet.size.asInstanceOf[Double]
    }

    println(authorsToPctTrainingSamples)

    val possibleAuthors = authorsToPctTrainingSamples.keys.toSet

    // instead of going through each word, get counts and then use them as exponents!

    def classifyNewDoc(docPath: String): BritishAuthor = {
      val fileText = getWordSequenceFromLocalFile(docPath)
      val wordCounts = getCounts(fileText)
      val authorsToLikelihoods = possibleAuthors map { possibleAuthor =>
        val logPAuthor = math.log(authorsToPctTrainingSamples(possibleAuthor))
        val logLikelihoods = wordCounts map { case (word, count) =>
          count *  logLikelihoodOfSeeingWordGivenAuthor(word, possibleAuthor)
        }
        (possibleAuthor, logLikelihoods.sum + logPAuthor)
      }
      println(authorsToLikelihoods)
      authorsToLikelihoods.maxBy{ _._2 }._1
    }

    { docPath => classifyNewDoc(docPath) }
  }

  val classifier: BritClassifier = getClassifierFromTrainingData(_)

  def main(args: Array[String]) {
    timed("Classified authors in %d ms" format _) {
      val trainedClassifier = classifier(trainingTextFileNamesAndClassifications)
      val classified = textFileNamesToClassify map { fileName =>
        (fileName, ("Classification\\" + fileName + ".txt") |> trainedClassifier)
      }
      println(classified)
    }
   }
}