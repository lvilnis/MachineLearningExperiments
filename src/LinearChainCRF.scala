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
  final case class Observation(obs: Seq[Int])
  final case class Label(labels: Seq[Int])
  final case class Instance(obs: Observation, labels: Label)
  final case class Weights(obs: Array[Double], markov: Array[Double] /*, bias: Seq[Double] */)
  case class ObsFactor(obsIndex: Int, obs: Int, markov: Int)
  case class BiasFactor(labelIndex: Int, label: Int)
  case class MarkovFactor(leftLabelIndex: Int, rightLabelIndex: Int, leftLabel: Int, rightLabel: Int)
  final case class Marginal(probs: IndexedSeq[Array[Array[Double]]]) {
    def prob(obs: Int, leftLabel: Int, rightLabel: Int): Double = probs(obs)(leftLabel)(rightLabel)
  }

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
    val labelDomain = Domain()
    words.foreach(obsDomain.indexForWord(_))
    labels.foreach(labelDomain.indexForWord(_))
    val markovWeights = Array(10.0, 2.0, 2.0, 10.0)
    val obsWeights = Array(10.0, 1.0, 1.0, 1.0, 10.0, 15.0)
    val weights = Weights(obsWeights, markovWeights)
    val generatedWords = generateInstance(weights, labelDomain, obsDomain).obs
    val mapEstimate = new Infer(weights, labelDomain, obsDomain).inferViterbi(generatedWords)
    println(mapEstimate.labels.map(labelDomain.wordForIndex(_)))

    val instances = for (_ <- 1 to 100) yield generateInstance(weights, labelDomain, obsDomain)

    val learner = new Learn(labelDomain, obsDomain)

    val learnedWeights = learner.learnWeights(instances)

    println("Obs weights:")
    learnedWeights.obs.foreach(println(_))
    println("Markov weights:")
    learnedWeights.markov.foreach(println(_))

    val infer = new Infer(weights, labelDomain, obsDomain)
    val mapEstimates = instances.map(_.obs).map(infer.inferViterbi(_))
    val labeledInstances = instances.map(_.obs).zip(mapEstimates).map({case (o, l) => Instance(o, l)})
    val matching = instances.zip(labeledInstances).filter({case (i, li) => i.labels.labels.sameElements(li.labels.labels)})
    println("Accuracy: %f" format ((matching.size * 1.0) / instances.size))
    labeledInstances.foreach(l => println(l.labels.labels.toSeq))
  }

  def generateInstance(weights: Weights, labelDomain: Domain, obsDomain: Domain): Instance = {
    val hiddenLabels = Seq("GOOD","GOOD","GOOD","GOOD","GOOD","GOOD","BAD","BAD","BAD","BAD","BAD", "GOOD", "GOOD")
    val hiddenLabelIndices = hiddenLabels.map(labelDomain.indexForWord(_))
    def generateWord(curLabel: Int): Int = sample(weights.obs.slice(curLabel * 3, curLabel * 3 + 3).zipWithIndex)
    Instance(Observation(hiddenLabelIndices.map(generateWord)), Label(hiddenLabelIndices))
  }

  class Learn(labelDomain: Domain, obsDomain: Domain, sgdPasses: Int = 5, learningRate: Double = 0.01, l2: Double = 0.01, initialState: Int = 0) {
    def learnWeights(instances: Seq[Instance]): Weights = {
      val weights = Weights(Array.fill(labelDomain.size * obsDomain.size)(0.0), Array.fill(labelDomain.size * labelDomain.size)(0.0))
      val infer = new Infer(weights, labelDomain, obsDomain)
      for (p <- 1 to sgdPasses; inst <- instances) {
        val marginal = infer.inferForwardBackward(inst.obs)
        val markovGradient = Array.fill(labelDomain.size * labelDomain.size)(0.0)
        val obsGradient = Array.fill(labelDomain.size * obsDomain.size)(0.0)
        for (o <- 0 until inst.obs.obs.size; i <- 0 until labelDomain.size; j <- 0 until labelDomain.size) {
          val curObs = inst.obs.obs(o)
          val curObsGradIdx = j * obsDomain.size + curObs
          val curMarkovGradIdx = j * labelDomain.size + i
          val curProb = math.exp(marginal.prob(curObs, i, j))
//          println("Marginal prob y_%d=%d y_%d=%d x_%d=%d is equal to %f" format (o - 1, i, o, j, o, curObs, curProb))
          obsGradient(curObsGradIdx) -= curProb
          markovGradient(curMarkovGradIdx) -= curProb
          if ((o == 0 && i == initialState && j == inst.labels.labels(o)) ||
              (o != 0 && i == inst.labels.labels(o - 1) && j == inst.labels.labels(o))) {
            obsGradient(curObsGradIdx) += 1
            markovGradient(curMarkovGradIdx) += 1
          }
        }
        // add by unregularized SGD for now
        for (i <- 0 until markovGradient.size) weights.markov(i) += (markovGradient(i) - l2) * learningRate
        for (i <- 0 until obsGradient.size) weights.obs(i) += (obsGradient(i) - l2) * learningRate
      }
      weights
    }
  }

  class Infer(weights: Weights, labelDomain: Domain, obsDomain: Domain, initialState: Int = 0) {
    @inline def score(leftLabel: Int, rightLabel: Int, rightObs: Int): Double = {
      val markovScore = weights.markov(labelDomain.size * leftLabel + rightLabel)
      val obsScore = weights.obs(obsDomain.size * rightLabel + rightObs)
//      println("Score: " + (markovScore + obsScore))
      markovScore + obsScore
    }

    @inline def overStates[A: Manifest](fun: Int => A): Array[A] = Array.tabulate(labelDomain.size)(fun)

    def inferForwardBackward(obs: Observation): Marginal = {
      def calculateNextForward(lastFwd: Array[Double], nextObs: Int): Array[Double] =
        overStates(j => fastSum(overStates(i => score(i, j, nextObs) + lastFwd(i))))
      val initialFwd = overStates(i => score(initialState, i, obs.obs(0)))
      val forwardVars = obs.obs.scanLeft(initialFwd)(calculateNextForward)

      def calculateLastBackward(lastObs: Int, nextBackward: Array[Double]): Array[Double] =
        overStates(i => fastSum(overStates(j => score(i, j, lastObs) + nextBackward(j))))
      val initialBwd = overStates(_ => 0.0)
      val backwardVars = obs.obs.scanRight(initialBwd)(calculateLastBackward)

//      println("forward vars")
//      forwardVars.map(_.toSeq).foreach(println)
//      println("backward vars")
//      backwardVars.map(_.toSeq).foreach(println)

      val marginals = for (o <- 1 until forwardVars.size) yield
        overStates(i => overStates(j => forwardVars(o - 1)(i) + score(i, j, obs.obs(o - 1)) + backwardVars(o)(j)))

//      marginals.foreach(_.foreach(_.foreach(println)))

      Marginal(marginals)
    }

    def inferViterbi(obs: Observation): Label = {
      def calculateNextViterbi(lastViterbi: Array[(Double, Int)], nextObs: Int): Array[(Double, Int)] =
        overStates(j => overStates(i => (score(i, j, nextObs) + lastViterbi(i)._1, i)).maxBy(_._1))
      val initialViterbi = overStates(i => (score(initialState, i, obs.obs(0)), -1))
      val viterbis = obs.obs.scanLeft(initialViterbi)(calculateNextViterbi)
      val lastV = viterbis.last
      val restV = viterbis.drop(2)
      val (_, finalBackpointer) = lastV.maxBy(_._1)
      val mapAssignment = restV.scanRight(finalBackpointer)((arr, ptr) => arr(ptr)._2)
      Label(mapAssignment)
    }
  }
}