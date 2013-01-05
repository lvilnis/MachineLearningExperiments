import LukeUtils._

object MachineLearningUtils {

  // General function to grab the first-order transitions of a sequence, regularized in a particular way
  def getConsecutivePairs[T](items: Seq[T]): Seq[(T, T)] =
    items.zip(items.drop(1))

  // This just strips out all numbers, turns all whitespace to single space, and makes lowercase
  def cleanupString(rawStr: String): String =
    rawStr.replaceAll("\\s+", " ").filter(c => c.isLetter || c.isSpaceChar).toLowerCase

  def getConsecutiveLetterOrSpacePairs(codedMsg: String): Seq[(Char, Char)] =
    getConsecutivePairs(cleanupString(codedMsg))

  def getWordSequenceFromString(str: String): Seq[String] =
    str.toLowerCase.filter(c => c.isLetter || c.isSpaceChar).split("\\s+")

  def getCounts[T](items: Seq[T]): Map[T, Int] = {
    val iterator = items.iterator
    val counts = new scala.collection.mutable.HashMap[T, Int]
    while (iterator.hasNext) {
      val current = iterator.next()
      counts.update(current, counts.getOrElse(current, 0) + 1)
    }
    counts.toMap
  }

  def getCountsDouble[T](items: Seq[T]): Map[T, Double] =
    getCounts(items).mapValues(_.asInstanceOf[Double])

  def getWordsAndCountsFromLocalFile(fileName: String): Map[String, Int] =
    getCounts(getWordSequenceFromLocalFile(fileName))

  def getWordSequenceFromLocalFile(fileName: String): Seq[String] =
    readLocalTextFile(fileName) |> getWordSequenceFromString

  def scramble[A](xs: Seq[A]): Seq[A] =
    xs.sortBy(_ => scala.math.random)

  def randomlySwapTwoMapEntries[T, U](map: Map[T, U]): Map[T, U] = {
    if (map.keys.toSeq.lengthCompare(2) < 0) return map
    // eww, slow... just pick two random keys, don't sort all of them!!
    val Seq(k1, k2) = scramble(map.keys.toSeq).take(2)
    map.updated(k1, map(k2)).updated(k2, map(k1))
  }

  def generateRandomPermutationMap[T](elements: Set[T]): Map[T, T] = {
    val elementList = elements.toSeq
    elementList.zip(scramble(elementList)).toMap
  }

  type Dist[+T] = () => T

  def dist[T](body: => T): Dist[T] = () => {body }

  def uniform: Dist[Double] = dist { scala.math.random }

  trait DistWrapper[+T] {
    def d: Dist[T]

    def flatMap[U](mapper: T => Dist[U]): Dist[U] = dist { mapper(d())() }

    def map[U](mapper: T => U): Dist[U] = dist { mapper(d()) }
  }

  implicit def dist2Wrapper[T](toWrap: Dist[T]): DistWrapper[T] = new DistWrapper[T] {
    val d = toWrap
  }

  @inline def logSumExp(arr: Array[Double]): Double = {
    val max = arr.max
    var sum = 0.0
    var i = 0
    while (i < arr.length) {
      sum += math.exp(arr(i) - max)
      i += 1
    }
    max + math.log(sum)
  }

  def sample(weightedCases: Seq[Double]): Int = {
    val cases = weightedCases.zipWithIndex.map(_._2)
    val weights = weightedCases
    val summedWeights = weights.scan(0d)(_ + _).drop(1)
    val sumOfWeights = summedWeights.last
    val probs = summedWeights.map(_ / sumOfWeights)
    val casesAndProbs = cases.zip(probs)
    val roll = uniform()
    val (caseChoice, _) = casesAndProbs.find({ case (_, prob) => prob > roll }).get
    caseChoice
  }

  def getWeightedCasesDistribution[T](weightedCases: Seq[(T, Double)]): Dist[T] = {
    val cases = weightedCases.map(_._1)
    val weights = weightedCases.map(_._2)
    val summedWeights = weights.scan(0d)(_ + _).drop(1)
    val sumOfWeights = summedWeights.last
    val probs = summedWeights.map(_ / sumOfWeights)
    val casesAndProbs = cases.zip(probs)
    for {
      roll <- uniform
      (caseChoice, _) = casesAndProbs.find({ case (_, prob) => prob > roll }).get
    } yield caseChoice
  }

  // A Classifier is a function which given a set of
  // labeled training data produces a function which labels new data
  type Classifier[Data, Label] = Set[(Data, Label)] => Data => Label
  type BinaryClassifier[Data] = Classifier[Data, Boolean]
  type DocumentClassifier[Label] = Classifier[String, Label]

}