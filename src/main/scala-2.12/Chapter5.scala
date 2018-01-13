/**
  * Created by bettori on 9/4/2017.
  */
import Chapter5.Stream._
object Chapter5 {
  sealed trait Stream[+A] {
    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    // Ex 5.1
    def toList : List[A] = {
      this match {
        case Empty => Nil
        case Cons(h, t) => h() :: t().toList
      }
    }

    // Ex 5.2
    def take(n: Int): Stream[A] = {
      this match {
        case Empty => Empty
        case Cons(h, t) => if (n > 0) Cons(h, () => t().take(n-1)) else Empty
      }
    }

    def drop(n: Int) : Stream[A] = {
      this match {
        case Empty => Empty
        case Cons(h, t) => if(n > 0) t().drop(n-1) else Cons(h, t)
      }
    }

    // Ex 5.3
    def takeWhile(f: A => Boolean): Stream[A] = {
      this match {
        case Empty => Empty
        case Cons(h, t) => if(f(h())) Cons(h, () => t().takeWhile(f)) else Empty
      }
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B = {
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }
    }

    // if p(a) returns true, b will never be evaluated, because it is lazily evaluated
    // p is the method that must be applied to each element in the stream, recursively.
    def exists(p : A => Boolean): Boolean = {
      foldRight(false)((a, b) => p(a) || b)
    }

    // Ex 5.4
    def forAll(p : A => Boolean): Boolean = {
      foldRight(true)((a, b) => p(a) && b)
    }

    // Ex 5.5
    def takeWhileFolded(f: A => Boolean): Stream[A] = {
      foldRight(Empty: Stream[A])((a, b) => if (f(a)) Cons( () => a,() => b) else Empty)
    }
    // Ex 5.6
    def headOptionFolded : Option[A] = {
      foldRight(None: Option[A])((a, _) => Some(a))
    }

    // Ex 5.7
    def append[B>:A](a: => Stream[B]): Stream[B] = {
      foldRight(a)((x, y) => Cons(() => x, () => y))
    }

    def map[B>:A](f: A => B): Stream[B] = {
      foldRight(Empty: Stream[B])((a, b) => Cons(() => f(a), () => b))
    }

    def flatMap[B>:A](f: A => Stream[B]): Stream[B] = {
      foldRight(Empty: Stream[B])((a, b) =>f(a).append(b))
    }

    def filter(f: A => Boolean) : Stream[A] = {
      foldRight(Empty: Stream[A])((a, b) => if(f(a)) Cons(() => a, () => b) else b)
    }

    def find(p: A => Boolean): Option[A] = {
      filter(p).headOption
    }

    // Ex 5.8
    // stream generation
    def constant[A](a: => A): Stream[A] = {
      lazy val tail: Stream[A] = Cons(() => a, () => tail)
      tail
    }

    // Ex 5.9

    // Ex 5.10
    def fibs(): Stream[Int] = {
      def f_fib (x: Int, y: Int): Stream[Int] = {
        Cons(() => x, () => f_fib(y, x+y))
      }

      f_fib(0, 1)
    }

    // Ex 5.11


    // Ex 5.12
    def fibs_unfold(): Stream[Int] = {
      unfold((0, 1))(a => Option((a._1, (a._2, a._1+a._2))))
    }

    def constant_unfold[A](a: => A): Stream[A] = {
      unfold(a)(a_n => Option((a_n,a_n)))
    }

    def from_unfold(n: Int): Stream[Int] = {
      unfold(n)(num => Option((num, num+1)))
    }

    // Ex 5.13
    // Syntax below between {...} is equivalent to : s => s match {...}
    def map_unfold[B>:A](f: A => B): Stream[B] = {
      unfold(this){
        case Cons(h, t) => Some((f(h()), t()))
        case _ => None
      }
    }

    def take_unfold(n: Int): Stream[A] = {
      unfold((this, n)){
        case (Cons(h, _), 0) => Option((h(), (Empty, 0)))
        case (Cons(h, t), k) if k > 1 => Option((h(), (t(), k-1)))
        case _ => None
      }
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
      unfold((this, s2)) {
        case (Cons(ha, ta), Cons(hb, tb)) => Option(((Some(ha()), Some(hb())), (ta(), tb())) )
        case _ => Option(((None: Option[A], None: Option[B]), (Empty, Empty)))
      }
    }

    def zip[B](s2: Stream[B]): Stream[(A, B)] = {
      unfold((this, s2)) {
        case (Cons(ha, ta), Cons(hb, tb)) => Some((ha(), hb()), (ta(), tb()))
        case _ => None
      }
    }

    // Ex 5.14
    def startsWith[A](s: Stream[A]): Boolean = {
      val stream = unfold((this, s)) {
        case (Cons(h1, t1), Cons(h2, t2)) => Option((Some(h1()), Some(h2())), (t1(), t2()))
        case (Cons(h, t), Empty) => Option((Some(h()), None), (t(), Empty))
        case (Empty, Cons(h, t)) => Option((None, Some(h())), (Empty, t()))
        case (Empty, Empty) => Option((None, None), (Empty, Empty))
      }

      stream.takeWhile(c => c._2.isDefined).forAll(p => p match {
        case (h, hs) => h == hs
      })
    }

    // Ex 5.15
    def tails: Stream[Stream[A]] = {
      unfold(this){
        case Cons(h, t) => Option((Cons(h, t), t()))
        case Empty => None
      } append Stream(Empty)
    }

    def hasSubsequence[A](s: Stream[A]): Boolean = {
      tails exists (_ startsWith s)
    }

    // Ex 5.16
    // type B is necessary because get contravariant error message if use A
    def scanRight[B](s: B)(f: (A, => B) => B): Stream[B] = {
      unfold(tails) {
        case Cons(h, t) => Option((h().foldRight(s)(f), t()))
        case Empty => None
      }
    }
  }
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream extends {
    def from(n: Int): Stream[Int] = {
      Cons(() => n, () => from(n+1))
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
      f(z) match {
        case Some(v) => Cons(() => v._1, () => unfold(v._2)(f) )
        case None => Empty
      }
    }
    // smart constructor
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      // memoize the arguments so that they are not evaluated more than once.
      // lazy keyword will cache the value.
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    // This method allows us to write Stream(1, 2, 3) to construct a stream with elements 1, 2 and 3
    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  }

  def main(args: Array[String]): Unit = {
    val testStream = Stream(1, 2, 3)
    println(testStream.take(2).toList)
    println(testStream.drop(1).toList)
    println(testStream.exists(a => a > 0))
    println(testStream.forAll(a => a > 0))
    println(testStream.forAll(a => a > 1))
    println(testStream.takeWhileFolded(a => a < 3).toList)
    println(testStream.drop(1).headOptionFolded)
    println(testStream.map(x => x + 1).toList)

    // infinite stream
    println(testStream.constant(2).take(3).toList)
    println(Stream.from(3).take(4).toList)
    println(testStream.fibs().take(6).toList)
    println(testStream.fibs_unfold().take(6).toList)
    println(testStream.constant_unfold(2).take(3).toList)
    println(testStream.from_unfold(3).take(3).toList)
    println(testStream.startsWith(Stream(1, 2)))
    println(testStream.tails.toList)
    println(testStream.scanRight(0)(_+_).toList)

    val testStringStream = Stream("a", "b", "c")
    println(testStringStream.scanRight("")(_.concat(_)).toList)
  }

}
