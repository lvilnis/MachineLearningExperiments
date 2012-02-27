import scalala.library.Random
import scalala.tensor.dense.DenseMatrix
import scalala.tensor.Matrix

object CleanedUpCipher {

  val machineLearningPath = "C:\\Users\\Luke\\Desktop\\Files For Machine Learning\\";

  def readTextFile(path: String): String = {
    io.Source.fromFile(machineLearningPath + path).mkString
  }

  def calculateFirstOrderTransitions(str: String): (Map[(Char, Char), Double], Int) = {
    val transitionPairs = getConsecutiveLetterPairs(str)
    val weightedTransitions = transitionPairs groupBy identity mapValues { _.length }
    val theMap = weightedTransitions mapValues { n => Math.log((n.toDouble + 1) / transitionPairs.length) }
    (theMap, transitionPairs.length)
  }

  val alphabet = Array('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',' ')

  def letter2digit(l: Char) = (alphabet indexOf l) + 1
  def digit2letter(d: Int) = alphabet(d - 1)

  def turnFirstOrderTransitionsIntoMatrix(trans: Map[(Char, Char), Double], numPairs: Int): Matrix[Double] = {
    val m = DenseMatrix.zeros[Double](27, 27)
    for (x <- 1 to 27;
         y <- 1 to 27;
         xl = digit2letter(x);
         yl = digit2letter(y)) {
      // we add 1 to every slot to avoid issues w/ multiplying by zeros, so default value is log(1 / numPairs)
      m(x - 1, y - 1) = trans getOrElse ((xl, yl), Math.log(1f / numPairs))
    }
    m
  }

  def getFirstOrderTransitionMatrixFromFile(fileName: String): Matrix[Double] = {
    val fileText = readTextFile(fileName)
    val (transitionMap, numPairs) = calculateFirstOrderTransitions(fileText)
    println(transitionMap.toSeq sortBy (kvp => kvp._1))
    val matrix = turnFirstOrderTransitionsIntoMatrix(transitionMap, numPairs)
    println(matrix)
    matrix
  }

  def getConsecutiveLetterPairs(codedMsg: String): Seq[(Char, Char)] = {
    val sanitizedMsg = codedMsg filter { c => c.isLetter || c.isSpaceChar } toLowerCase;
    sanitizedMsg zip (sanitizedMsg drop 1)
  }

  def getLogPlausibilityOfSubstitutionCipher(transitionMatrix: Matrix[Double])(codedMsg: String, c: SubstitutionCipher): Double = {
    // safe to use substitution cipher since we've eliminated all punc/spaces and toLower'd it
    getConsecutiveLetterPairs(codedMsg)
      .map { case (s1, s2) => transitionMatrix(letter2digit(c(s1)) - 1, letter2digit(c(s2)) - 1) }
      .reduce { _ + _ }
    // we use + instead of * because we've converted to logs
  }

  def randomlyTransposeCipher(c: SubstitutionCipher): SubstitutionCipher = {
    val Seq(l1, l2) = scramble(1 to 27) take 2 map digit2letter
    c updated (l1, c(l2)) updated (l2, c(l1))
  }


    type SubstitutionCipher = Map[Char, Char]

  type CipherTranslator = Function[Char, Char]

  // this just fills in the things we don't want to consider like punctuation and spaces
  def translatorFromCipher(c: SubstitutionCipher): CipherTranslator = {
    c orElse { case c: Char => c }
  }

  def scramble[A](xs: Seq[A]): Seq[A] =
    xs sortBy { _ => Random.rand()(Random.mt) }

  def generateRandomCipher(): SubstitutionCipher = {
    var m = Map.empty[Char, Char]
    for ((i, o) <- (1 to 27) zip (scramble (1 to 27));
         input = digit2letter(i);
         output = digit2letter(o)) {
      m = m updated (input, output)
    }
    m
  }

  def invertCipher(c: SubstitutionCipher): SubstitutionCipher = {
    val inv = c map { case (a, b) => (b, a) }
    println(inv)
    inv
  }

  def algorithmFromTheDoc() {
    // Ingredients:
    // 1. A nice transition matrix from a big old english document:
    val transitionMatrixFromEnglish = getFirstOrderTransitionMatrixFromFile("WarAndPeace.txt")
    // 2. A substitution cipher you want to figure out:
    // 3. A document coded in some substitution cipher you wanna figure out:
    val randomCipher = generateRandomCipher()
    val encodedDocument = readTextFile("OscarWildePoems.txt") map (translatorFromCipher(randomCipher))

    def logPl(c: SubstitutionCipher): Double =
      getLogPlausibilityOfSubstitutionCipher(transitionMatrixFromEnglish)(encodedDocument, c)

    // Start with a preliminary guess, say f
    val fPrelim = generateRandomCipher()

    def loop(f: SubstitutionCipher, i: Int, fBest: SubstitutionCipher, fBestPlausibility: Double): SubstitutionCipher = {
            // Compute Pl(f)
      val fPlausibility = logPl(f)
      val fStar = randomlyTransposeCipher(f)
      val fStarPlausibility = logPl(fStar)

      // to get ratio of logs, you subtract and exp
      val ratio = Math.exp(fStarPlausibility - fPlausibility);

      

      val fNew = if (ratio >= 1) fStar else if (Random.rand()(Random.mt) <= ratio) fStar else f

      if (ratio isNaN) {
        println("FPlausibility: %f FStarPlausibility: %f" format (fPlausibility,  fStarPlausibility))
        return f
      }

      val fBestNew = if (fStarPlausibility > fBestPlausibility) fStar else fBest
      val fBestPlausibilityNew = if (fStarPlausibility > fBestPlausibility) fStarPlausibility else fBestPlausibility

      if (i < 10000)
        loop(fNew, i + 1, fBestNew, fBestPlausibilityNew)
      else
        fBestNew
    }

    val soln = loop(fPrelim, 0, fPrelim, logPl(fPrelim))

    println("=== FINAL ANSWER ===")
    println(encodedDocument map translatorFromCipher(soln))
  }


  def main(args: Array[String]) {
    algorithmFromTheDoc()
  }
}