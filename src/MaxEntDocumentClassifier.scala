import LukeUtils._
import MachineLearningUtils._

object MaxEntDocumentClassifier {

  import Classification._

  def getClassifierFromTrainingData(trainingSet: Set[(String, BritishAuthor)]): String => BritishAuthor = {

    def classifyNewDoc(docPath: String): BritishAuthor = {
      val fileText = getWordSequenceFromLocalFile(docPath)
      val wordCounts = getCounts(fileText)

      sys.error("unimplemented")
    }

    classifyNewDoc(_)
  }

  val classifier: BritClassifier = getClassifierFromTrainingData(_)

  def main(args: Array[String]) {
    timed("(MaxEnt) Classified authors in %d ms" format _) {
      val trainedClassifier = classifier(trainingTextFileNamesAndClassifications)
      val classified = textFileNamesToClassify map { fileName =>
        (fileName, ("Classification\\" + fileName + ".txt") |> trainedClassifier)
      }
      println(classified)
    }
  }

}