package simplified

object PartialFunction {

  def plus(a: Int)(b: Int) : Int = a + b

  // example of partially applied function
  def plus2: Int => Int = plus(2) _

  def wrap(prefix: String)(html: String)(suffix: String) : String = {
    prefix + html + suffix
  }

  def wrapWithPre = wrap("<pre>")(_ : String)("</pre>")



  def main(args: Array[String]) = {
    val result = plus2(3)
    println(result)

    val preResult = wrapWithPre("Hello")
    println(preResult)
  }


}

