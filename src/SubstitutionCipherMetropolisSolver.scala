import annotation.tailrec
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.Matrix
import LukeUtils._
import MachineLearningUtils._

object SubstitutionCipherMetropolisSolver {

  val printDebug = false

  def calculateFirstOrderTransitions(str: String): (Map[(Char, Char), Double], Int) = {
    val transitionPairs = getConsecutiveLetterOrSpacePairs(str)
    val weightedTransitions = transitionPairs groupBy identity mapValues { _.length }
    val theMap = weightedTransitions mapValues { n => scala.math.log((n.toDouble + 1) / transitionPairs.length) }
    (theMap, transitionPairs.length)
  }

  val alphabet = Array('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',' ')

  def letter2digit(l: Char): Int = (alphabet indexOf l) + 1
  def digit2letter(d: Int): Char = alphabet(d - 1)

  def turnFirstOrderTransitionsIntoMatrix(trans: Map[(Char, Char), Double], defaultValue: Double): Matrix[Double] = {
    val m = DenseMatrix.zeros[Double](27, 27)
    for { x <- 1 to 27
          y <- 1 to 27
          xl = digit2letter(x)
          yl = digit2letter(y)
    } m(x - 1, y - 1) = trans getOrElse ((xl, yl), defaultValue)
    m
  }

  def getFirstOrderTransitionMatrixFromFile(fileName: String): Matrix[Double] = {
    val fileText = readLocalTextFile(fileName)
    val (transitionMap, numPairs) = calculateFirstOrderTransitions(fileText)
    if (printDebug) { println(transitionMap.toSeq sortBy { case (key, _) => key }) }
    // we add 1 to every slot to avoid issues w/ multiplying by zeros, so default value is log(1 / numPairs)
    val matrix = turnFirstOrderTransitionsIntoMatrix(transitionMap, scala.math.log(1f / numPairs))
    if (printDebug) { println(matrix) }
    matrix
  }

  type SubstitutionCipher = Map[Char, Char]

  type CipherTranslator = Char => Char

  // this just fills in the things we don't want to consider like punctuation and spaces
  def translatorFromCipher(c: SubstitutionCipher): CipherTranslator =
    c orElse { case c: Char => c }

  def randomlyTransposeCipher(c: SubstitutionCipher): SubstitutionCipher =
    randomlySwapTwoMapEntries(c)
  
  def generateRandomCipher(): SubstitutionCipher =
    generateRandomPermutationMap(alphabet toSet)

  def invertCipher(c: SubstitutionCipher): SubstitutionCipher =
    c map { case (a, b) => (b, a) }

  def getLogPlausibilityOfSubstitutionCipher(transitionMatrix: Matrix[Double])(codedMsg: String, c: SubstitutionCipher): Double = {
    getConsecutiveLetterOrSpacePairs(codedMsg)
      .map { case (s1, s2) => transitionMatrix(letter2digit(c(s1)) - 1, letter2digit(c(s2)) - 1) }
      .reduce { _ + _ }
    // we use + instead of * because we've converted to logs
  }
  
  // Uses Metropolis algorithm
  def findBestCandidateCipher(transitionMatrix: Matrix[Double], encodedDocument: String, numIterations: Int): SubstitutionCipher = {
    def logPl(c: SubstitutionCipher): Double =
      getLogPlausibilityOfSubstitutionCipher(transitionMatrix)(encodedDocument, c)

    // Start with a preliminary guess, say f
    val fPrelim = generateRandomCipher()

    @tailrec
    def loop(f: SubstitutionCipher, i: Int, fBest: SubstitutionCipher, fBestPlausibility: Double): SubstitutionCipher = {
      val fPlausibility = logPl(f)
      val fCandidate = randomlyTransposeCipher(f)
      val fCandidatePlausibility = logPl(fCandidate)

      // to get ratio of logs, you subtract and exp
      val ratio = scala.math.exp(fCandidatePlausibility - fPlausibility);

      val fNew =
        if (ratio >= 1 || scala.math.random <= ratio) fCandidate
        else f

      val (fBestNew, fBestPlausibilityNew) =
        if (fCandidatePlausibility > fBestPlausibility) (fCandidate, fCandidatePlausibility)
        else (fBest, fBestPlausibility)

      if (i < numIterations) loop(fNew, i + 1, fBestNew, fBestPlausibilityNew)
      else fBestNew
    }

    loop(fPrelim, 0, fPrelim, logPl(fPrelim))
  }

  def main(args: Array[String]) {
    val transitionMatrixFromEnglish = getFirstOrderTransitionMatrixFromFile("SubstitutionCipher\\WarAndPeace.txt")
    val randomCipher = generateRandomCipher()
    val encodedDocument = readLocalTextFile("SubstitutionCipher\\SampleTextToEncipher.txt") map (translatorFromCipher(randomCipher))

    val soln = findBestCandidateCipher(transitionMatrixFromEnglish, encodedDocument, 20000)

    println("=== FINAL ANSWER ===")
    println(encodedDocument map translatorFromCipher(soln))
  }
}