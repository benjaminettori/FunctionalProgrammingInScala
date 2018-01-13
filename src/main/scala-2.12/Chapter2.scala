import scala.annotation.tailrec

object Chapter2 {

  // Ex 2.1
  def fib(n: Int): Int = {
    @tailrec
    def iter(n: Int, prev: Int, curr: Int): Int = {
      if(n == 0) prev
      else iter(n-1, curr, prev + curr)
    }

    // n is the number of times to run the prev + curr sum. Shifting curr in the iteration allows us to return the result
    iter(n, 0, 1)
  }

  // Ex 2.2
  def isSorted[A](things: Array[A], ordered: (A, A) => Boolean): Boolean = {
    val maxN = things.length - 1

    def iter(n: Int): Boolean = {
      if(n+1 <= maxN) {
        if(ordered(things(n), things(n+1))) {
          if(n+1 < maxN) iter(n+1) else true
        } else false
      } else false
    }

    iter(0)
  }

  //Ex 2.3
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a,b)
  }

  // Ex 2.4
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  // Ex 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println(fib(5))
    println(isSorted(Array(1,3,2,4,5), (a: Int,b: Int) => a < b))
  }

}
