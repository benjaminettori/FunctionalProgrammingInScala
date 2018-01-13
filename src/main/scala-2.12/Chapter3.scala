/**
  * Created by bettori on 7/11/2017.
  */
object Chapter3 {

  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    // Ex 3.2
    def tail[T](items: List[T]): List[T] = {
      items match {
        case Nil => Nil
        case Cons(_, xs) => xs
      }
    }

    def head[T](items: List[T]): Option[T] = {
      items match {
        case Cons(x, _) => Some(x)
        case Nil => None
      }
    }

    // Ex 3.3
    def setHead[T](head: T, list: List[T]): List[T] = {
      list match {
        case Nil => Nil
        case Cons(_, xs) => Cons(head, xs)
      }
    }

    //Ex 3.4
    def drop[A](l: List[A], n: Int): List[A] = {
      l match {
        case Nil => Nil
        case Cons(_, xs) => if (n == 1) xs else drop(xs, n - 1)
      }
    }

    //Ex 3.5
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
      l match {
          // Note use of pattern guard in this case statement
        case Cons(h, xs) if f(h) => dropWhile(xs, f)
        case _ => l
      }
    }

    // Ex 3.6

    def init[A](l: List[A]): List[A] = {
      l match {
        case Nil => Nil
        case Cons(_, Nil) => Nil
        case Cons(h, tail) => Cons(h, init(tail))
      }
    }

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B ={
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }
    }

    // Ex 3.10
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
      as match {
        case Nil => z
        case Cons(v, xs) => foldLeft(xs, f(z, v))(f)
      }
    }

    def sum2(ns: List[Int]) = foldRight(ns, 0)((x, y) => x + y)

    def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

    def length[A](ns: List[A]): Int = foldRight(ns, 0)((_, y) => y + 1)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
  }

  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // Ex 3.1
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + List.sum(t)
    case _ => 101
  }

  def main(args: Array[String]): Unit = {
    val testList = List(1, 2, 3, 4, 5)
    println(x)
    println(List.tail(testList))
    println(List.drop(testList, 2))
    println(List.init(testList))

    // Ex. 3.7
    println(List.product2(List(1, 0, 4)))

    // Ex 3.8
    println(List.foldRight(testList, Nil: List[Int])(Cons(_, _)))

    // Ex 3.9
    println(List.length(testList))

    // Ex 3.11
    // Sum
    println(List.foldLeft(testList, 0)(_+_))

    // Product
    println(List.foldLeft(testList, 1)(_*_))

    // Length
    println(List.foldLeft(testList, 0)((y, _) => y + 1))

    // Ex 3.12
    def reverse[A](as : List[A]): List[A] = List.foldLeft(as, Nil: List[A])((l, v) => Cons(v, l))
    println(reverse(testList))

    // Ex 3.13
     def foldRightSpecial[A, B](as: List[A], z: B)(f: (A, B) => B): B = List.foldLeft(reverse(as), z)((b, a) => f(a, b))

    // Ex 3.14
    def appendFold[A](as: List[A], bs: List[A]): List[A] = foldRightSpecial(as, bs)((a, l) => Cons(a, l))
    println(appendFold(testList, List(7, 8, 9)))

    // Ex 3.15
    def flatten[A](als: List[List[A]]): List[A] = foldRightSpecial(als, Nil: List[A])((a, b) => appendFold(a, b))
    println(flatten(List(List(1, 2), List(3, 4))))

    // Ex 3.16
    def addOne(a: List[Int]): List[Int] = {
      foldRightSpecial(a, Nil: List[Int])((x, y) => Cons(x+1, y))
    }

    println(addOne(testList))

    // Ex 3.17
    def doubleToString(a: List[Int]): List[String] = foldRightSpecial(a, Nil: List[String])((x, y) => Cons(x.toString, y))
    println(doubleToString(testList))

    // Ex 3.18
    def map[A, B](a: List[A])(f: A => B): List[B] = foldRightSpecial(a, Nil: List[B])((x, y) => Cons(f(x), y))
    println(map(testList)(x => x.toString))

    // Ex 3.19
    def filter[A](a: List[A])(f: A => Boolean): List[A] = foldRightSpecial(a, Nil: List[A])((xa, acc) => if(f(xa)) Cons(xa, acc) else acc)
    println(filter(testList)(a => a % 2 == 0))

    // Ex 3.20
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRightSpecial(as, Nil: List[B])((a, lb) => appendFold(f(a), lb))
    println(flatMap(testList)(x => List(x, x)))

    def flatMap_2[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

    // Ex 3.21
    def filterFlatMap[A](a: List[A])(f: A => Boolean): List[A] = flatMap_2(a)(e => if(f(e)) List(e) else List())
    println(filterFlatMap(testList)(a => a % 2 == 0))

    // Ex 3.22
    def combineLists(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
      case (Cons(x, xs), Cons(y, ys)) => Cons(x+y, combineLists(xs, ys))
      case (Nil, _) => Nil
      case (_, Nil) => Nil
    }
    // Ex 3.23
    def zipWith[A](xl: List[A], yl: List[A])(f: (A, A) => A): List[A] = (xl, yl) match {
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
      case (Nil, _) => Nil
      case (_, Nil) => Nil
    }

    // Ex 3.24

    def startsWith[A](l: List[A], pref: List[A]): Boolean = {
      (l, pref) match {
        case (Cons(h,hs), Cons(t, ts)) if h == t => startsWith(hs, ts)
        case (_, Nil) => true
        case _ => false
      }
    }

    def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
      sub match {
        case Nil => true
        case Cons(h, t) => if(startsWith(sup, List(h))) hasSubSequence(List.tail(sup), t) else hasSubSequence(List.tail(sup), sub)
        case _ => startsWith(sup, sub)
      }
    }

    println(hasSubSequence(testList, List(3,4)))

    val testTree = Branch[Int](Branch[Int](Leaf[Int](3), Leaf[Int](4)), Leaf[Int](5))

    // Ex 3.25
    def size[A](t: Tree[A]): Int = {
      t match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
      }
    }

    println(size(testTree))

    // Ex 3.26
    def max(t: Tree[Int]): Int = {
      t match {
        case Leaf(x) => x
        case Branch(l, r) => max(l) max max(r)
      }
    }

    println(max(testTree))

    // Ex 3.27
    def depth[A](t: Tree[A]): Int = {
      t match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + (depth(l) max depth(r))
      }
    }

    println(depth(testTree))

    // Ex 3.28
    def mapTree[A, B](t: Tree[A])(f: A => B): Tree[B] = {
      t match {
        case Leaf(x) => Leaf(f(x))
        case Branch(l, r) => Branch(mapTree(l)(f), mapTree(r)(f))
      }
    }

    println(mapTree(testTree)(x => x > 3))

//    def fold[A, B](a: Tree[A])(f: A => B)(g: (B, B) => B): B ={
//      a match {
//        case Leaf(x) => f(x)
//        case Branch(l, r) => g(fold(l)(f), fold(r)(f))
//      }
//    }
//
//    def depthFold[A](t: Tree[A]): Int = fold(t)(a => 0)((d1, d2) => 1 +  (d1 max d2))
//    def mapFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(a => Leaf(f(a)): Tree[B])((t1, t2) => Branch(t1, t2))
  }
}
