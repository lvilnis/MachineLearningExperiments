import java.io.File
import scala.collection.generic.CanBuildFrom
import scala.collection.{IterableLike, mutable}

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

  trait ArrayWrapper {
    protected def arr: Array[Double]
    def +(other: ArrayWrapper): ArrayWrapper = {
      val output = Array.fill(arr.length)(0.0)
      var i = 0
      while (i < arr.length) {
        output(i) = arr(i) + other.arr(i)
        i += 1
      }
      wrapArray(output)
    }
    def *(other: ArrayWrapper): ArrayWrapper = {
      val output = Array.fill(arr.length)(0.0)
      var i = 0
      while (i < arr.length) {
        output(i) = arr(i) * other.arr(i)
        i += 1
      }
      wrapArray(output)
    }
    def *(other: Double): ArrayWrapper = {
      val output = Array.fill(arr.length)(0.0)
      var i = 0
      while (i < arr.length) {
        output(i) = arr(i) * other
        i += 1
      }
      wrapArray(output)
    }
    def +=(other: ArrayWrapper): this.type = {
      var i = 0
      while (i < arr.length) {
        arr(i) += other.arr(i)
        i += 1
      }
      this
    }
    def -=(other: ArrayWrapper): this.type = {
      var i = 0
      while (i < arr.length) {
        arr(i) -= other.arr(i)
        i += 1
      }
      this
    }
    def *=(other: ArrayWrapper): this.type = {
      var i = 0
      while (i < arr.length) {
        arr(i) *= other.arr(i)
        i += 1
      }
      this
    }
    def *=(other: Double): this.type = {
      var i = 0
      while (i < arr.length) {
        arr(i) *= other
        i += 1
      }
      this
    }
  }

  implicit def wrapArray(a: Array[Double]): ArrayWrapper = new ArrayWrapper { def arr = a }

  def mutate(x: Array[Double], y: Array[Double], fun: (Double, Double) => Double): Unit = {
    var i = 0
    while (i < x.length) {
      x(i) = fun(x(i), y(i))
      i += 1
    }
  }

  def add(x: Array[Double], y: Array[Double]): Unit = {
    var i = 0
    while (i < x.length) {
      x(i) += y(i)
      i += 1
    }
  }

  def maxIndex[A, B: Ordering](xs: Iterable[A])(fun: A => B): Int = xs.zipWithIndex.maxBy(t => fun(t._1))._2
  def maxAndIndex[A: Ordering](xs: Iterable[A]): (A, Int) = xs.zipWithIndex.maxBy(t => t._1)

  def average(xs: Iterable[Double]): Double = xs.sum / xs.size

  def countSame[A](xs: Iterable[A], ys: Iterable[A]): Int = map2(xs, ys)(_ == _).count(identity)
  def pctSame[A](xs: Iterable[A], ys: Iterable[A]): Double = countSame(xs, ys) * 1.0 / xs.size

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

  def map2[A, B, C, This[A] <: Iterable[A]](xs: This[A], ys: Iterable[B])(fun: (A, B) => C)
    (implicit bf: CanBuildFrom[This[C], C, This[C]]): This[C] = {
    val xit = xs.iterator
    val yit = ys.iterator
    val output =  bf()
    while (xit.hasNext && yit.hasNext) {
      output += (fun(xit.next(), yit.next()))
    }
    output.result()
  }

  def map3[A, B, C, D, This[A] <: Iterable[A]](xs: This[A], ys: Iterable[B], zs: Iterable[C])(fun: (A, B, C) => D)
    (implicit bf: CanBuildFrom[This[D], D, This[D]]): This[D] = {
    val xit = xs.iterator
    val yit = ys.iterator
    val zit = zs.iterator
    val output =  bf()
    while (xit.hasNext && yit.hasNext && zit.hasNext) {
      output += (fun(xit.next(), yit.next(), zit.next()))
    }
    output.result()
  }
}