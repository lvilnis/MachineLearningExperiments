import LukeUtils._
import MachineLearningUtils._

object NaiveBayesDocumentClassifier {

  // a naive classifier simply counts the frequency of certain
  // words, and figures out the probability that something contains a word,
  // given that it belongs to a class
  // Then it uses Bayes' rule to calculate the probability that something
  // has a label, given it contains certain words

  // What's good training data for this? Maybe project gutenberg authors
  // and books?

  // NOTE: I should make a little helper to download texts from gutenberg for various authors and/or genres
  // is there a gutenberg web api or should i just scrape?

  abstract sealed class BritishAuthor
  case object OscarWilde extends BritishAuthor
  case object CharlesDickens extends BritishAuthor
  
  type BritClassifier = DocumentClassifier[BritishAuthor]

  val trainingTextFileNamesAndClassifications: Set[(String, BritishAuthor)] =
    Set(("AChristmasCarol", CharlesDickens),
        ("OliverTwist", CharlesDickens),
        ("TheHappyPrinceAndOtherTales", OscarWilde),
        ("ThePictureOfDorianGray", OscarWilde))

  val textFileNamesToClassify =
    Set("GreatExpectations",
        "TaleOfTwoCities",
        "ImportanceOfBeingErnest",
        "TheCantervilleGhost")

  def getClassifierFromTrainingData(set: Set[(String, BritishAuthor)]): String => BritishAuthor = {
    // calculate probabilities
    // get list of probability, given author, of word
    // Map[BritishAuthor, Map[String, Double]] s.t. doubles add up to one... maybe they should be logs...
    // so go through every occurence of a word and count how many for each author
    // s.t. for each word, sum over all authors of map(author)(word) = 1

   
    error("asd")
  }

 // def givenProbabilitiesOfWordGivenCategoryGetProbabilityOfCategoryGivenWord(probs: )

  val classifier: BritClassifier = getClassifierFromTrainingData(_)

  def main(args: Array[String]) {
    val trainedClassifier = classifier(trainingTextFileNamesAndClassifications)
    val classified = textFileNamesToClassify map { fileName =>
      (fileName, readLocalTextFile("Classifiers\\" + fileName) |> trainedClassifier)
    }
    println(classified)
  }
}