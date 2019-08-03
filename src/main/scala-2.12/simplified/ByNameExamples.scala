package simplified

object ByNameExamples {

  def simpleMethod[A](codeBlock : => A) : Boolean = {
    try {
      codeBlock
      true
    } catch {
      case _: Exception => false
    }
  }

  def whilst(predicate : => Boolean)(codeBlock : => Unit) : Unit = {
    if(predicate) {
      codeBlock
      whilst(predicate)(codeBlock)
    }
  }

  // Implementing C# using pattern
  // Note use of the anonymous type defining close method
  // Normally would use trait here
  def using[A <: {def close(): Unit}, B](resource: A)(f: A => B) : B = {
    try {
      f(resource)
    } finally {
      resource.close()
    }
  }

  def main(args: Array[String]) : Unit = {
    val isSuccess = simpleMethod {
      val a = 1
      val b = 2
      println(a + b)
    }

    println(isSuccess)

    val isFailure = simpleMethod {
      throw new Exception("Fail")
    }

    println(isFailure)

    var i: Int = 0
    whilst(i < 5) {
      println(i)
      i += 1
    }

    using(io.Source.fromFile("test.txt")) { file =>
      val lines = file.getLines()
      for (line <- lines) {
        println(line)
      }
    }

  }

}
