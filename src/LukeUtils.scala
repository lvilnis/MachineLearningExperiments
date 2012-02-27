import java.io.File

object LukeUtils {

  def readTextFile(path: String): String = {
    io.Source.fromFile(path).mkString
  }

  def writeTextFile(path: String, contents: String) {
    def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
      val p = new java.io.PrintWriter(f)
      try { op(p) } finally { p.close() }
    }
    printToFile(new File(path)) { p =>
      p.print(contents)
    }
  }

  val localPath = "C:\\Users\\Luke\\Desktop\\Files For Machine Learning\\";

  def readLocalTextFile(path: String): String = {
    readTextFile(localPath + path)
  }

  def writeLocalTextFile(path: String, contents: String) {
    writeTextFile(localPath + path, contents)
  }

  case class Pipeable[A](a: A) {
    def |>[B](f: A => B) = f(a)
  }

  implicit def convert[A](s: A) = Pipeable(s)



}