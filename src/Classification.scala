import MachineLearningUtils._

object Classification {

  abstract sealed class BritishAuthor

  case object OscarWilde extends BritishAuthor

  case object CharlesDickens extends BritishAuthor

  type BritClassifier = DocumentClassifier[BritishAuthor]

  // note to self: use "Dispatch" library to download from Project Gutenberg directly....

  val trainingTextFileNamesAndClassifications: Set[(String, BritishAuthor)] =
    Set(("GreatExpectations", CharlesDickens),
      ("TaleOfTwoCities", CharlesDickens),
      ("TheHappyPrinceAndOtherTales", OscarWilde),
      ("ThePictureOfDorianGray", OscarWilde))

  val textFileNamesToClassify =
    Set("AChristmasCarol",
      "OliverTwist",
      "ImportanceOfBeingErnest",
      "TheCantervilleGhost")
}
