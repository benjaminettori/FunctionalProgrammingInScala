import scala.annotation.tailrec

object Trampolining {

  // In order to avoid getting stack overflow errors when recursing on data structures
  // need tail call elimination

  // This can be achieved by defining a control structure that is tail recursive in order to
  // benefit from the JVM's limited implementation of tail call elimination (only works on tail recursive methods)

  // SimpleTrampoline has limitations, if have need to call n-ary recursion (n calls to recursive method)
  // In that case, stack overflow would still occur
  sealed trait SimpleTrampoline[A] {
    // run is tail recursive
    def run : A = this match {
      case SimpleDone(a) => a
      case SimpleMore(f) => f().run
    }
  }

  case class SimpleDone[A](a: A) extends SimpleTrampoline[A]
  case class SimpleMore[A](a: () => SimpleTrampoline[A]) extends SimpleTrampoline[A]

  // Example of usage

  def even(n : Int): SimpleTrampoline[Boolean] = {
    if(n == 0) SimpleDone(true) else SimpleMore(() => odd(n-1))
  }

  def odd(n: Int): SimpleTrampoline[Boolean] = {
    if(n == 0) SimpleDone(false) else SimpleMore(() => even(n - 1))
  }


  // In order to generalize the simple trampoline definition above, need to make it a monad
  sealed trait Trampoline[A] {


    // Note resume and run are tail-recursive

    def resume: Either[() => Trampoline[A], A] = {
      this match {
        case Done(a) => Right(a)
        case More(f) => Left(f)
        case FlatMap(tr, mapper: (Any => Trampoline[A])) => tr match {
          case Done(a1) => mapper(a1).resume
          case More(g) => Left(() => g().flatMap(mapper))
          case FlatMap(b, g: (Any => Trampoline[Any])) => b.flatMap((x: Any) => g(x).flatMap(mapper)).resume
        }
      }
    }

    def run: A = resume match {
      case Right(a) => a
      case Left(f) => f().run
    }

    def flatMap[B](f: A => Trampoline[B]) : Trampoline[B] = {
      this match {
//        case FlatMap(tr, g: (Any => Trampoline[A])) => FlatMap(tr, (x: Any) =>  g(x).flatMap(f))
        case FlatMap(tr, g: (Any => Trampoline[A])) => tr.flatMap((x: Any) => FlatMap(g(x), f))
        case x => FlatMap(x, f)
      }
    }

    def map[B](f: A => B): Trampoline[B] = this.flatMap((a: A) => Done(f(a)))
  }

  // Example of usage
  // For large values of n, regular recursion for fibonacci could cause a stack overflow
  // Since Trampoline's run method is tail recursive, we know tail call elimination will happen here

  def fibonacci(n: Int) : Trampoline[Int] = {
    if (n <= 1) Done(n)
    else for {
      x <- fibonacci(n-1)
      y <- fibonacci(n-2)
    } yield x + y
  }

  case class Done[A](a: A) extends Trampoline[A]
  case class More[A](a: () => Trampoline[A]) extends Trampoline[A]

  // make FlatMap constructor private to force usage of flatMap method instead of instantiating new FlatMap objects
  // This is to avoid nesting of FlatMap objects, which could cause a stack overflow
  case class FlatMap[A, B] private (t: Trampoline[A], mapper: A => Trampoline[B]) extends Trampoline[B]

  def main(args: Array[String]): Unit = {
    println(even(101).run)
    println(fibonacci(100).run)
  }


}
