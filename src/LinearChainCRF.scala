import annotation.tailrec
import LukeUtils._
import MachineLearningUtils._
import scala.collection.mutable

trait Domain {
  def indexForWord(word: String): Int
  def wordForIndex(index: Int): String
  def size: Int
}

object Domain {
  def apply(words: String*): Domain = new Domain {
    var size = 0
    private val vocab = new mutable.HashMap[String, Int]
    words.foreach(indexForWord)
    def indexForWord(word: String): Int = vocab.get(word) match {
      case Some(idx) => idx
      case None =>
        vocab(word) = size
        size += 1
        size - 1
    }
    def wordForIndex(idx: Int): String =
      vocab.find(_._2 == idx).getOrElse(sys.error("Can't find word for idx!"))._1
  }
}

object LinearChainCRF {
  final case class Observation(obs: Seq[Int])
  final case class Label(labels: Seq[Int])
  final case class Instance(obs: Observation, labels: Label) {
    def length = labels.labels.length
  }
  final case class Weights(obs: Array[Double], markov: Array[Double], bias: Array[Double]) {
    def +=(other: Weights): this.type = { obs += other.obs; markov += other.markov; bias += other.bias; this }
    def -=(other: Weights): this.type = { obs -= other.obs; markov -= other.markov; bias -= other.bias; this }
    def dot(other: Weights): Double = (other.obs dot obs) + (other.markov dot markov) + (other.bias dot bias )
  }
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

    val obsDomain = Domain("FOO", "BAR", "BAZ" /*, "QUX"*/)
    val labelDomain = Domain("GOOD", "BAD" /*, "HUH"*/)
    def printLabels(l: Label): Unit = println(l.labels.map(labelDomain.wordForIndex))
    val markovWeights = Array(10.0, 2.0, 2.0, 10.0 /*, 10.0, 5.0, 2.0, 10.0, 5.0*/)
    val obsWeights = Array(10.0, 1.0, 1.0, 1.0, 10.0, 15.0 /*, 20.0, 7.0, 2.0, 2.0, 3.0, 20.0*/)
    val biasWeights = Array(5.0, 3.0, 1.0)
    val weights = Weights(obsWeights, markovWeights, biasWeights)
    val generatedWords = generateInstance(weights, labelDomain, obsDomain, 20, 0)
    println("actual labels:")
    printLabels(generatedWords.labels)
    val mapEstimate = new Infer(weights, labelDomain, obsDomain).inferViterbi(generatedWords.obs)
    println("map estimate:")
    printLabels(mapEstimate)

    val instances = for (_ <- 1 to 100) yield generateInstance(weights, labelDomain, obsDomain, 5, 0)

    val learner = new Learn(labelDomain, obsDomain)

    val learnedWeights = learner.learnWeightsStructuredSVM(instances)

    println("Obs weights:")
    learnedWeights.obs.foreach(println)
    println("Markov weights:")
    learnedWeights.markov.foreach(println)
    println("Bias weights:")
    learnedWeights.bias.foreach(println)

    val infer = new Infer(learnedWeights, labelDomain, obsDomain)
    val mapEstimates = instances.map(_.obs).map(infer.inferViterbi)
//    mapEstimates.map(_.labels.toSeq).foreach(println)
    val labeledInstances = map2(instances.map(_.obs), mapEstimates)(Instance)
    val matching = map2(instances, labeledInstances)((i, li) => pctSame(i.labels.labels, li.labels.labels))
    println("Accuracy: %f" format average(matching))
//    labeledInstances.foreach(l => println(l.labels.labels.toSeq))
  }

  def generateInstance(weights: Weights, labelDomain: Domain, obsDomain: Domain, instanceLength: Int, initialState: Int): Instance = {
    val hiddenLabelIndices = Stream.iterate(initialState, instanceLength)(state =>
      sample(weights.markov.slice(state * labelDomain.size, state * labelDomain.size + labelDomain.size))).toArray
    def generateWord(curLabel: Int): Int =
      sample(weights.obs.slice(curLabel * obsDomain.size, curLabel * obsDomain.size + obsDomain.size))
    Instance(Observation(hiddenLabelIndices.map(generateWord)), Label(hiddenLabelIndices))
  }

  trait CrfHelpers {
    def initialState = 0
    def labelDomain: Domain
    def obsDomain: Domain
    def blankWeights: Weights = Weights(
      Array.fill(labelDomain.size * obsDomain.size)(0.0),
      Array.fill(labelDomain.size * labelDomain.size)(0.0),
      Array.fill(labelDomain.size)(0.0))

    @inline final def obsStatIdx(curState: Int, curObs: Int) = curState * obsDomain.size + curObs
    @inline final def markovStatIdx(prevState: Int, curState: Int) = curState * labelDomain.size + prevState
    @inline final def biasStatIdx(curState: Int) = curState
    @inline final def getSufficientStatistics(instance: Instance, initialState: Int = 0): Weights = {
      val Weights(obsStats, markovStats, biasStats) = blankWeights
      var prevState = initialState
      val numObs = instance.obs.obs.size
      var o = 0
      while (o < numObs) {
        val curObs = instance.obs.obs(o)
        val curState = instance.labels.labels(o)
        markovStats(markovStatIdx(prevState, curState)) += 1
        obsStats(obsStatIdx(curState, curObs)) += 1
        biasStats(biasStatIdx(curState)) += 1
        prevState = curState
        o += 1
      }
      Weights(obsStats, markovStats, biasStats)
    }
  }

  class Learn(val labelDomain: Domain, val obsDomain: Domain, sgdPasses: Int = 20,
    learningRate: Double = 0.01, l2: Double = 0.01) extends CrfHelpers {

    def learnWeightsMaxLikelihood(instances: Seq[Instance]): Weights = {
      val weights = blankWeights
      val infer = new Infer(weights, labelDomain, obsDomain)
      for (p <- 1 to sgdPasses; inst <- instances) {
        val marginal = infer.inferForwardBackward(inst.obs)
        val expectedSufficientStats = blankWeights
        for (o <- 0 until inst.obs.obs.size; i <- 0 until labelDomain.size; j <- 0 until labelDomain.size) {
          val curObs = inst.obs.obs(o)
          val curProb = math.exp(marginal.prob(curObs, i, j))
//          println("Marginal log prob y_%d=%d y_%d=%d x_%d=%d is equal to %f" format (o - 1, i, o, j, o, curObs, marginal.prob(curObs, i, j)))
          expectedSufficientStats.obs(obsStatIdx(j, curObs)) += curProb
          expectedSufficientStats.markov(markovStatIdx(i, j)) += curProb
          expectedSufficientStats.bias(biasStatIdx(j)) += curProb
        }
        val gradient = getSufficientStatistics(inst)
        gradient -= expectedSufficientStats
        takeGradientStep(weights, gradient)
      }
      weights
    }

    def takeGradientStep(weights: Weights, gradient: Weights): Unit = {
      for (i <- 0 until gradient.markov.size) weights.markov(i) += (gradient.markov(i) - l2 * weights.markov(i)) * learningRate
      for (i <- 0 until gradient.obs.size) weights.obs(i) += (gradient.obs(i) - l2 * weights.obs(i)) * learningRate
      for (i <- 0 until gradient.bias.size) weights.bias(i) += (gradient.bias(i) - l2 * weights.bias(i)) * learningRate
    }

    def learnWeightsPerceptron(instances: Seq[Instance]): Weights = {
      val weights = blankWeights
      val infer = new Infer(weights, labelDomain, obsDomain)
      for (p <- 1 to sgdPasses; inst <- instances) {
        val map = infer.inferViterbi(inst.obs)
        if (!map.labels.sameElements(inst.labels.labels)) {
          val gradient = blankWeights
          gradient += getSufficientStatistics(inst)
          gradient -= getSufficientStatistics(inst.copy(labels = map))
          takeGradientStep(weights, gradient)
        }
      }
      weights
    }

    def learnWeightsStructuredSVM(instances: Seq[Instance]): Weights = {
      val weights = blankWeights
      val infer = new Infer(weights, labelDomain, obsDomain)
      for (p <- 1 to sgdPasses; inst <- instances) {
        val (mapLabel, mapScorePlusLoss) = infer.inferLossAugmentedViterbi(inst)
        val hammingLoss =  inst.length - countSame(mapLabel.labels, inst.labels.labels)
        // try to make sure margin of score of ground-truth over best-wrong labeling is > hammingloss
        val groundTruthScore = getSufficientStatistics(inst) dot weights
        if (groundTruthScore <= mapScorePlusLoss) {
          // we need to make an update
          val gradient = blankWeights
          gradient += getSufficientStatistics(inst)
          gradient -= getSufficientStatistics(inst.copy(labels = mapLabel))
          takeGradientStep(weights, gradient)
        }
      }
      weights
    }
  }

  class Infer(weights: Weights, val labelDomain: Domain, val obsDomain: Domain) extends CrfHelpers {

    @inline def score(leftLabel: Int, rightLabel: Int, rightObs: Int): Double =
      weights.markov(markovStatIdx(leftLabel, rightLabel)) +
      weights.obs(obsStatIdx(rightLabel, rightObs)) +
      weights.bias(biasStatIdx(rightLabel))

    @inline def overStates[A: Manifest](fun: Int => A): Array[A] = Array.tabulate(labelDomain.size)(fun)

    def inferMaxMarginal(obs: Observation): Label = {
      val labelRange = 0 until labelDomain.size
      val factorMarginals = inferForwardBackward(obs)
      def getLabelMarginal(fm: Array[Array[Double]]) = labelRange.map(j => labelRange.map(fm(_)(j)).sum)
      val labelMarginals = factorMarginals.probs.map(getLabelMarginal)
      Label(labelMarginals.map(maxAndIndex(_)).map(_._2))
    }

    def inferForwardBackward(obs: Observation): Marginal = {
      def calculateNextForward(lastFwd: Array[Double], nextObs: Int): Array[Double] =
        overStates(j => logSumExp(overStates(i => score(i, j, nextObs) + lastFwd(i))))
      val initialFwd = overStates(i => score(initialState, i, obs.obs(0)))
      val forwardVars = obs.obs.scanLeft(initialFwd)(calculateNextForward)

      def calculateLastBackward(lastObs: Int, nextBackward: Array[Double]): Array[Double] =
        overStates(i => logSumExp(overStates(j => score(i, j, lastObs) + nextBackward(j))))
      val initialBwd = overStates(_ => 0.0)
      val backwardVars = obs.obs.scanRight(initialBwd)(calculateLastBackward)

//      println("forward vars")
//      forwardVars.map(_.toSeq).foreach(println)
//      println("backward vars")
//      backwardVars.map(_.toSeq).foreach(println)

      val marginals = for (o <- 1 until forwardVars.size) yield
        overStates(i => overStates(j => forwardVars(o - 1)(i) + score(i, j, obs.obs(o - 1)) + backwardVars(o)(j)))

      // need to normalize log probs
      for (m <- marginals; logZ = logSumExp(m)) m.foreach(_ -= logZ)
      Marginal(marginals)
    }

    def calculateNextViterbi(lastViterbi: Array[(Double, Int)], nextObs: Int): Array[(Double, Int)] =
      overStates(j => maxAndIndex(overStates(i => (score(i, j, nextObs) + lastViterbi(i)._1))))

    def inferViterbi(obs: Observation): Label = {
      val initialViterbi = overStates(i => (score(initialState, i, obs.obs(0)), -1))
      val viterbis = obs.obs.scanLeft(initialViterbi)(calculateNextViterbi)
      val finalState = maxIndex(viterbis.last)(_._1)
//      viterbis.foreach(arr => println(arr.toSeq))
      val mapAssignment = viterbis.drop(2).scanRight(finalState)((arr, ptr) => arr(ptr)._2)
      Label(mapAssignment)
    }

    @inline def scoreLossAugmented(leftLabel: Int, rightLabel: Int, idx: Int, inst: Instance): Double =
      score(leftLabel, rightLabel, inst.obs.obs(idx)) + (if (rightLabel != inst.labels.labels(idx)) 1.0 else 0.0)

    def calculateNextLossAugmentedViterbi(lastViterbi: Array[(Double, Int)], obsIdx: Int, inst: Instance): Array[(Double, Int)] =
      overStates(j => maxAndIndex(overStates(i => (scoreLossAugmented(i, j, obsIdx, inst) + lastViterbi(i)._1))))

    def inferLossAugmentedViterbi(inst: Instance): (Label, Double) = {
      val initialViterbi = overStates(i => (scoreLossAugmented(initialState, i, 0, inst), -1))
      val viterbis = (0 until inst.length).scanLeft(initialViterbi)(calculateNextLossAugmentedViterbi(_, _, inst))
      val finalState = maxIndex(viterbis.last)(_._1)
      val mapAssignment = viterbis.drop(2).scanRight(finalState)((arr, ptr) => arr(ptr)._2)
      (Label(mapAssignment), viterbis.last.map(_._1).max)
    }
  }
}