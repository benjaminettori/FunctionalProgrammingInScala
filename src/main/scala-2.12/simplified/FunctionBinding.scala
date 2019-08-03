package simplified

object FunctionBinding {
  def bind(f: Int => (Int, String), res: (Int, String)) : (Int, String) = {
    val (gInt, gString) = f(res._1)
    (gInt, res._2 + gString)
  }

  trait Wrapper[+A] {
    def map[B](f: A => B) : Wrapper[B]
    def flatMap[B](f: A => Wrapper[B]) : Wrapper[B]
  }

  case class SimpleWrapper[A](value: A) extends Wrapper[A]{
    def map[B](f: A => B): SimpleWrapper[B] = SimpleWrapper(f(value))
    def flatMap[B](f: A => Wrapper[B]) : Wrapper[B] = f(value)
    override def toString: String = value.toString
  }

  case class Debuggable[A](value: A, message: String) extends Wrapper[A] {
    override def map[B](f: A => B): Debuggable[B] = Debuggable(f(value), message)

    override def flatMap[B](f: A => Wrapper[B]): Debuggable[B] = {
      val intermediateResult = f(value)
      intermediateResult match {
        case Debuggable(v, m) => Debuggable(v, m + message)
        case SimpleWrapper(v) => Debuggable(v, message)
      }
    }

    override def toString: String = value.toString
  }

  def main(args: Array[String]) = {
    val result = for {
      a <- SimpleWrapper((1, "Hello"))
      b <- SimpleWrapper((2, "Hello"))
      c <- SimpleWrapper((3, "Hello"))
    } yield a._1 + b._1 + c._1

    println(result)

  }


}
