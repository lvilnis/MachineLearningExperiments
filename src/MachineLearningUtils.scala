
object MachineLearningUtils {

  // General function to grab the first-order transitions of a sequence, regularized in a particular way
  def getConsecutivePairs[T](codedMsg: Seq[T])(mapper: T => T, predicate: T => Boolean): Seq[(T, T)] = {
    val sanitizedMsg = codedMsg filter predicate map mapper
    sanitizedMsg zip (sanitizedMsg drop 1)
  }

  def getConsecutiveLetterOrSpacePairs(codedMsg: String): Seq[(Char, Char)] =
    getConsecutivePairs(codedMsg)(_.toLower, c => c.isLetter || c.isSpaceChar)

  def scramble[A](xs: Seq[A]): Seq[A] =
    xs sortBy { _ => scala.math.random }

  def randomlySwapTwoMapEntries[T, U](map: Map[T, U]): Map[T, U] = {
    if (map.keys.toSeq.lengthCompare(2) < 0) return map;
    // eww, slow... just pick two random keys, don't sort all of them!!
    val Seq(k1, k2) = scramble(map.keys.toSeq) take 2
    map updated (k1, map(k2)) updated (k2, map(k1))
  }

  def generateRandomPermutationMap[T](elements: Set[T]): Map[T, T] = {
    val elementList = elements.toSeq
    elementList zip scramble(elementList) toMap
  }
}