import java.io.File

object LukeUtils {

  def readTextFile(path: String): String =
    io.Source.fromFile(path).mkString

  def writeTextFile(path: String, contents: String): Unit = {
    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit): Unit = {
      val p = new java.io.PrintWriter(f)
      try op(p) finally p.close()
    }
    printToFile (new File(path)) { f =>
      f.print(contents)
    }
  }

  val localPath = "inputs\\"

  def readLocalTextFile(path: String): String =
    readTextFile(localPath + path)

  def writeLocalTextFile(path: String, contents: String): Unit =
    writeTextFile(localPath + path, contents)

  class Pipeable[A](a: A) {
    def |>[B](f: A => B) = f(a)
    def <|:[B](f: A => B) = f(a)
  }

  class Vectorable[A](a: Iterable[A]) {
    def toVector: Vector[A] = Vector(a.toSeq: _*)
  }

  implicit def convert[A](s: A) = new Pipeable(s)
  implicit def convert[A](s: Iterable[A]) = new Vectorable(s)

  def groupPairsByFirst[A, B](set: Seq[(A, B)]): Map[A, Seq[B]] =
    set.groupBy(_._1).mapValues(_.toSeq.map(_._2))

  def timed[T](showTime: Long => String)(body: => T) = {
      val start = System.currentTimeMillis
      val result = body
      println(showTime(System.currentTimeMillis - start))
      result
  }

  def add(x: Array[Double], y: Array[Double]): Unit = {
    var i = 0
    while (i < x.length) {
      x(i) += y(i)
      i += 1
    }
  }

  @inline def logSumExp(arr: Array[Double]): Double = {
    val max = arr.max
    var sum = max
    var i = 0
    while (i < arr.length) {
      sum += math.exp(arr(i) - max)
      i += 1
    }
    sum
  }

  // why do I have to write this myself, again? come on, Scala...
  @inline def fastSum(arr: Array[Double]): Double = {
    var sum = 0.0
    var topic = 0
    while (topic < arr.length) {
      sum += arr(topic)
      topic += 1
    }
    sum
  }
}