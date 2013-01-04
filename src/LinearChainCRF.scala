import annotation.tailrec
import LukeUtils._
import MachineLearningUtils._
import scala.collection.mutable

trait Domain {
  def indexForWord(word: String): Int
  def wordForIndex(index: Int): String
  def freeze(): Unit
  def size: Int
}

object Domain {
  def apply(): Domain = new Domain {
    var size = 0
    private var frozen = false
    private val vocab = new mutable.HashMap[String, Int]
    def indexForWord(word: String): Int = vocab.get(word) match {
      case Some(idx) => idx
      case None if frozen => sys.error("Frozen!")
      case None =>
        vocab(word) = size
        size += 1
        size - 1
    }
    def wordForIndex(idx: Int): String =
      vocab.find(_._2 == idx).getOrElse(sys.error("Can't find word for idx!"))._1
    def freeze(): Unit = frozen = true
  }
}

object LinearChainCRF {
  case class Observation(obs: Seq[Int])
  case class Label(labels: Seq[Int])
  case class Instance(obs: Observation, labels: Label)
  case class Weights(obs: Seq[Double], markov: Seq[Double] /*, bias: Seq[Double] */)
  case class ObsMarkovFactor(obsIndex: Int, obs: Int, markov: Int)
  case class BiasFactor(labelIndex: Int, label: Int)
  case class MarkovFactor(leftLabelIndex: Int, rightLabeIndex: Int, leftLabel: Int, rightLabel: Int)

  def main(args: Array[String]): Unit = {
    // Do this stuff later, assume we're given labeled instances
//    val inputFilePath = args(0)
//    val inputString = readTextFile(inputFilePath)
//    val sentenceStrings = inputString.split("\\.").toSeq.map(getWordSequenceFromString)
//    val obsDomain = Domain()
//    val sentences = sentenceStrings.map(_.map(obsDomain.indexForWord(_)))

    val words = Seq("FOO", "BAR", "BAZ")
    val labels = Seq("GOOD", "BAD")
    val obsDomain = Domain()
    val markovDomain = Domain()
    words.foreach(obsDomain.indexForWord(_))
    labels.foreach(markovDomain.indexForWord(_))
    val markovWeights = Array(10.0, 2.0, 10.0, 2.0)
    val obsWeights = Array(10.0, 1.0, 1.0, 1.0, 10.0, 15.0)
    val weights = Weights(obsWeights, markovWeights)
    val hiddenLabels = Seq("GOOD","GOOD","GOOD","GOOD","GOOD","GOOD","BAD","BAD","BAD","BAD","BAD", "GOOD", "GOOD")
    val hiddenLabelIndices = hiddenLabels.map(markovDomain.indexForWord(_))
    def generateWord(curLabel: Int): Int = sample(obsWeights.slice(curLabel * 3, curLabel * 3 + 3).zipWithIndex)
    val generatedWords = hiddenLabelIndices.map(generateWord)
    println(hiddenLabels)
    println(generatedWords.map(obsDomain.wordForIndex(_)))
    val mapEstimate = infer(Observation(generatedWords), weights, markovDomain, obsDomain)
    println(mapEstimate.labels.map(markovDomain.wordForIndex(_)))
  }

  def learnWeights(instances: Seq[Instance], labelDomain: Domain, obsDomain: Domain): Weights = {
    sys.error("unimpl")
  }

  def infer(obs: Observation, weights: Weights, labelDomain: Domain, obsDomain: Domain): Label = {
    val initialState = 0
    val lookup = Array.fill(labelDomain.size * labelDomain.size * obsDomain.size)(Double.NaN)

    @inline def score(leftLabel: Int, rightLabel: Int, rightObs: Int): Double = {
      val lookupIdx = obsDomain.size * labelDomain.size * leftLabel + labelDomain.size * rightLabel + rightObs
      val score = lookup(lookupIdx)
      if (!score.isNaN) score
      else {
        val markovScore = weights.markov(labelDomain.size * leftLabel + rightLabel)
        val obsScore = weights.obs(obsDomain.size * rightLabel + rightObs)
        val totalScore = markovScore + obsScore
        lookup(lookupIdx) = totalScore
        totalScore
      }
    }

    @inline def overStates[A: Manifest](fun: Int => A): Array[A] = Array.tabulate(labelDomain.size)(fun)

    def calculateNextForward(lastFwd: Array[Double], nextObs: Int): Array[Double] =
      overStates(j => fastSum(overStates(i => score(i, j, nextObs) + lastFwd(i))))
    val initialFwd = overStates(i => score(initialState, i, obs.obs(0)))
    val forwardVars = obs.obs.scanLeft(initialFwd)(calculateNextForward)

    def calculateLastBackward(lastObs: Int, nextBackward: Array[Double]): Array[Double] =
      overStates(i => fastSum(overStates(j => score(i, j, lastObs) + nextBackward(j))))
    val initialBwd = overStates(_ => 1.0)
    val backwardVars = obs.obs.scanRight(initialBwd)(calculateLastBackward).reverse

    def calculateNextViterbi(lastViterbi: Array[(Double, Int)], nextObs: Int): Array[(Double, Int)] =
      overStates(j => overStates(i => (score(i, j, nextObs) + lastViterbi(i)._1, i)).maxBy(_._1))
    val initialViterbi = initialFwd.map(f => (f, 0))
    val viterbis = obs.obs.scanLeft(initialViterbi)(calculateNextViterbi)
    val lastV = viterbis.last
    val restV = viterbis.dropRight(1)
    val finalAssignment = lastV.zipWithIndex.maxBy(_._1._1)
    val finalBackpointer = finalAssignment._1._2
    val finalIndex = finalAssignment._2
    val mapAssignment = restV.scanRight(finalBackpointer)((arr, ptr) => arr(ptr)._2)
    Label(mapAssignment.drop(1))
  }
}