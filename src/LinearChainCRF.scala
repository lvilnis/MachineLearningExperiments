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
    def printLabels(l: Label): Unit = println(l.labels.map(labelDomain.wordForIndex(_)))
    words.foreach(obsDomain.indexForWord(_))
    labels.foreach(labelDomain.indexForWord(_))
    val markovWeights = Array(10.0, 2.0, 2.0, 10.0)
    val obsWeights = Array(10.0, 1.0, 1.0, 1.0, 10.0, 15.0)
    val weights = Weights(obsWeights, markovWeights)
    val generatedWords = generateInstance(weights, labelDomain, obsDomain, 13, 0)
    println("actual labels:")
    printLabels(generatedWords.labels)
    val mapEstimate = new Infer(weights, labelDomain, obsDomain).inferViterbi(generatedWords.obs)
    println("map estimate:")
    printLabels(mapEstimate)

//    return

    val instances = for (_ <- 1 to 100) yield generateInstance(weights, labelDomain, obsDomain, 25, 0)

    val learner = new Learn(labelDomain, obsDomain)

    val learnedWeights = learner.learnWeightsPerceptron(instances)

    println("Obs weights:")
    learnedWeights.obs.foreach(println(_))
    println("Markov weights:")
    learnedWeights.markov.foreach(println(_))

    val infer = new Infer(learnedWeights, labelDomain, obsDomain)
    val mapEstimates = instances.map(_.obs).map(infer.inferViterbi(_))
//    mapEstimates.map(_.labels.toSeq).foreach(println)
    val labeledInstances = map2(instances.map(_.obs), mapEstimates)(Instance(_, _))
    val matching = map2(instances, labeledInstances)((i, li) => pctSame(i.labels.labels, li.labels.labels))
    println("Accuracy: %f" format average(matching))
//    labeledInstances.foreach(l => println(l.labels.labels.toSeq))
  }

  def generateInstance(weights: Weights, labelDomain: Domain, obsDomain: Domain, instanceLength: Int, initialState: Int): Instance = {
    val hiddenLabelIndices = Stream.iterate(initialState, instanceLength)(state =>
      sample(weights.markov.slice(state * labelDomain.size, state * labelDomain.size + labelDomain.size))).toArray
//    val hiddenLabels = Seq("GOOD","GOOD","GOOD","GOOD","GOOD","GOOD","BAD","BAD","BAD","BAD","BAD", "GOOD", "GOOD")
//    val hiddenLabelIndices = hiddenLabels.map(labelDomain.indexForWord(_))
    def generateWord(curLabel: Int): Int =
      sample(weights.obs.slice(curLabel * obsDomain.size, curLabel * obsDomain.size + obsDomain.size))
    Instance(Observation(hiddenLabelIndices.map(generateWord)), Label(hiddenLabelIndices))
  }

  trait CrfHelpers {
    def initialState = 0
    def labelDomain: Domain
    def obsDomain: Domain
    @inline final def obsStatIdx(curState: Int, curObs: Int) = curState * obsDomain.size + curObs
    @inline final def markovStatIdx(prevState: Int, curState: Int) = curState * labelDomain.size + prevState
    @inline final def getSufficientStatistics(instance: Instance, initialState: Int = 0): (Array[Double], Array[Double]) = {
      val markovStats = Array.fill(labelDomain.size * labelDomain.size)(0.0)
      val obsStats = Array.fill(labelDomain.size * obsDomain.size)(0.0)
      var prevState = initialState
      val numObs = instance.obs.obs.size
      var o = 0
      while (o < numObs) {
        val curObs = instance.obs.obs(o)
        val curState = instance.labels.labels(o)
        markovStats(markovStatIdx(prevState, curState)) = 1
        obsStats(obsStatIdx(curState, curObs)) = 1
        prevState = curState
        o += 1
      }
      (obsStats, markovStats)
    }
  }

  class Learn(val labelDomain: Domain, val obsDomain: Domain, sgdPasses: Int = 20,
    learningRate: Double = 0.01, l2: Double = 0.01) extends CrfHelpers {
    def learnWeightsMaxLikelihood(instances: Seq[Instance]): Weights = {
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
          println("Marginal log prob y_%d=%d y_%d=%d x_%d=%d is equal to %f" format (o - 1, i, o, j, o, curObs, marginal.prob(curObs, i, j)))
          obsGradient(curObsGradIdx) -= curProb
          markovGradient(curMarkovGradIdx) -= curProb
        }
        val (obsStats, markovStats) = getSufficientStatistics(inst)
        add(markovGradient, markovStats)
        add(obsGradient, obsStats)
        println("Obs grad:" + obsGradient.toSeq)
        println("Markov grad:" + markovGradient.toSeq)
        // add by SGD for now
        for (i <- 0 until markovGradient.size) weights.markov(i) += (markovGradient(i) - l2 * weights.markov(i)) * learningRate
        for (i <- 0 until obsGradient.size) weights.obs(i) += (obsGradient(i) - l2 * weights.obs(i)) * learningRate
      }
      weights
    }

    def learnWeightsPerceptron(instances: Seq[Instance]): Weights = {
      val weights = Weights(Array.fill(labelDomain.size * obsDomain.size)(0.0), Array.fill(labelDomain.size * labelDomain.size)(0.0))
      val infer = new Infer(weights, labelDomain, obsDomain)
      for (p <- 1 to sgdPasses; inst <- instances) {
        val map = infer.inferViterbi(inst.obs)
        val markovGradient = Array.fill(labelDomain.size * labelDomain.size)(0.0)
        val obsGradient = Array.fill(labelDomain.size * obsDomain.size)(0.0)
        if (!map.labels.sameElements(inst.labels.labels)) {
          val (obsStats, markovStats) = getSufficientStatistics(inst)
          add(markovGradient, markovStats)
          add(obsGradient, obsStats)
        }
//        println("Obs grad:" + obsGradient.toSeq)
//        println("Markov grad:" + markovGradient.toSeq)
        // add by SGD for now
        for (i <- 0 until markovGradient.size) weights.markov(i) += (markovGradient(i) - l2 * weights.markov(i)) * learningRate
        for (i <- 0 until obsGradient.size) weights.obs(i) += (obsGradient(i) - l2 * weights.obs(i)) * learningRate
      }
      weights
    }
  }

  class Infer(weights: Weights, val labelDomain: Domain, val obsDomain: Domain) extends CrfHelpers {

    @inline def score(leftLabel: Int, rightLabel: Int, rightObs: Int): Double = {
      val markovScore = weights.markov(markovStatIdx(leftLabel, rightLabel))
      val obsScore = weights.obs(obsStatIdx(rightLabel, rightObs))
//      println("Score: " + (markovScore + obsScore))
      markovScore + obsScore
    }

    @inline def overStates[A: Manifest](fun: Int => A): Array[A] = Array.tabulate(labelDomain.size)(fun)

    def inferForwardBackward(obs: Observation): Marginal = {
      def calculateNextForward(lastFwd: Array[Double], nextObs: Int): Array[Double] =
        overStates(j => logSumExp(overStates(i => score(i, j, nextObs) + lastFwd(i))))
      val initialFwd = overStates(i => score(initialState, i, obs.obs(0)))
      val forwardVars = obs.obs.scanLeft(initialFwd)(calculateNextForward)

      def calculateLastBackward(lastObs: Int, nextBackward: Array[Double]): Array[Double] =
        overStates(i => logSumExp(overStates(j => score(i, j, lastObs) + nextBackward(j))))
      val initialBwd = overStates(_ => 0.0)
      val backwardVars = obs.obs.scanRight(initialBwd)(calculateLastBackward)

      println("forward vars")
      forwardVars.map(_.toSeq).foreach(println)
      println("backward vars")
      backwardVars.map(_.toSeq).foreach(println)

      val marginals = for (o <- 1 until forwardVars.size) yield
        overStates(i => overStates(j => forwardVars(o - 1)(i) + score(i, j, obs.obs(o - 1)) + backwardVars(o)(j)))

//      marginals.foreach(_.foreach(_.foreach(println)))

      Marginal(marginals)
    }

    def inferViterbi(obs: Observation): Label = {
      def calculateNextViterbi(lastViterbi: Array[(Double, Int)], nextObs: Int): Array[(Double, Int)] =
        overStates(j => maxAndIndex(overStates(i => (score(i, j, nextObs) + lastViterbi(i)._1))))
      val initialViterbi = overStates(i => (score(initialState, i, obs.obs(0)), -1))
      val viterbis = obs.obs.scanLeft(initialViterbi)(calculateNextViterbi)
      val finalState = maxIndex(viterbis.last)(_._1)
//      viterbis.foreach(arr => println(arr.toSeq))
      val mapAssignment = viterbis.drop(2).scanRight(finalState)((arr, ptr) => arr(ptr)._2)
      Label(mapAssignment)
    }
  }
}