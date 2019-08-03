package simplified
import cats.effect.IO
import ByNameExamples.using

import scala.util.Try
import scala.util.Success
import scala.util.Failure


// In practice, IO is not really used, except for console IO
// Files, network, db don't typically use the IO monad
// They may use a type alias called IO, that is in fact a Try, for instance.
object IoMonad {

  def getLine: IO[String] = IO(scala.io.StdIn.readLine())
  def writeLine(s: String) : IO[Unit] = IO(println(s))

  def readTextFileAsTry(fileName: String) : IO[Try[List[String]]] = {
    IO(Try {
      val lines = using(io.Source.fromFile(fileName)) { source =>
        (for (line <- source.getLines) yield line).toList
      }
      lines
    })
  }

  def main(args: Array[String]) = {

    // this describes a side effect encapsulated in the IO monad
    // nothing is getting executed yet
    // Allows us to isolate side effects in the code
    
    val myEffect : IO[Unit] = for {
      _ <- writeLine("First Name")
      firstName <- getLine
      _ <- writeLine("Last Name")
      lastName <- getLine
      _ <- writeLine(s"Hello $firstName $lastName")
    } yield ()

    // myProgram is an IO construct that hasn't been executed yet
    // you can run it by calling a run function explicitly
    myEffect.unsafeRunSync()

    val passwordFile = readTextFileAsTry("test.txt")

    val test = for {
      result <- passwordFile
    } yield (
      result match {
        case Success(lines) => lines.foreach(println)
        case Failure(exception) => println(exception.getMessage)
    })

    test.unsafeRunSync()

  }

}
