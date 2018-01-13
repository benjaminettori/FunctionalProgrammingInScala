/**
  * Created by bettori on 9/13/2017.
  */
object Chapter6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  // Ex 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
   val (i, r) = rng.nextInt
    (if (i < 0) -(i+1) else i, r)
  }

  // Ex 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), r)
  }

  // Ex 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = rng.nextInt
    val (d, rd) = double(r)

    ((i, d), rd)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)

    ((d1, d2, d3), r3)
  }

  // Ex 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val (i, r) = rng.nextInt
    if (count > 1) {
      val next = ints(count -1)(r)
      (i :: next._1, next._2)
    } else {
      (List(i), r)
    }
  }

  type State[S, +A] = S => (A, S)

  type Rand[+A] = State[RNG, A]

  def unit[A](a: A): Rand[A] = {
    rng => (a, rng)
  }

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(i => i - i % 2)
  }

  // Ex 6.5
  def double_map: Rand[Double] = {
    map(nonNegativeInt)(i => i / Int.MaxValue)
  }

  // Ex 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rnda) = ra(rng)
      val (b, rndb) = rb(rnda)
      (f(a, b), rndb)
    }
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = {
    map2(ra, rb)((_, _))
  }

  def randIntDouble: Rand[(Int, Double)] = {
    both(nonNegativeInt, double)
  }

  def randDoubleInt: Rand[(Double, Int)] = {
    both(double, nonNegativeInt)
  }

  def trav[A](fa: List[Rand[A]], rng: RNG): List[A] = {
    fa match {
      case Nil => List[A]()
      case ha :: ta => {
        val (a, rnda) = ha(rng)
        a :: trav(ta, rnda)
      }
    }
  }

  // Ex 6.7
  def sequence[A](fa: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      (trav(fa, rng), rng)
    }
  }

  // List.fill takes Int as size of list, and lazy val to indicate what to fill with
  def ints_spec(count: Int): Rand[List[Int]] = {
    sequence(List.fill(count)(nonNegativeInt))
  }

  // Ex 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, r) = f(rng)
      g(a)(r)
    }
  }

  def nonNegativeLessThan(n: Int) : Rand[Int] = {

      def fn(ni: Int): Rand[Int] = {
        val mod = ni % n
        if(ni + (n-1) - mod >= 0) {
          rn => (mod, rn)
        } else {
          nonNegativeLessThan(n)
        }
    }

      flatMap(r => nonNegativeInt(r))(fn)
  }

  // Ex 6.9
  def map_new[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(a => {
      rng => (f(a), rng)
    })
  }

//  def  map2_new[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
//    flatMap(rng => {
//      val (a, rna) = ra(rng)
//      val (b, rnb) = rb(rna)
//      ((a, b), rnb)
//    })(d => {
//      rng => (f(d), rng)
//    })
//  }

  def map2_altern[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }

  // Ex 6.10
  def unit_state[A, S](a: A): State[S, A] = {
    s => (a, s)
  }

  def flat_map[A, B, S](f: State[S, A])(g: A => State[S, B]) : State[S, B] = {
    s => {
      val (a, sa) = f(s)
      g(a)(sa)
    }
  }

  def map_state[A, B, S](ra: State[S, A])(f: A => B): State[S, B] = {
    flat_map(ra)(a => {
      s => (f(a), s)
    })
  }

  def map2_state[A, B, C, S](ra: State[S, A], rb: State[S, B])(f: (A, B) => C): State[S, C] = {
    flat_map(ra)(a => map_state(rb)(b => f(a, b)))
  }

  def sequence_state[S, A](sas: List[State[S, A]]) : State[S, List[A]] = {
    sas.reverse.foldLeft(unit_state[List[A], S](List[A]()))((ra, rla) => map2_state(rla, ra)((a, b) => a :: b))
  }

  // Ex 6.11
  case class ScalaState[S, +A](run: S => (A, S)) {
    def flatMap[B](f: A => ScalaState[S, B]) : ScalaState[S, B] = ScalaState(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })

    def map[B](f: A => B) : ScalaState[S, B] = {
      flatMap(a => ScalaState.unit(f(a)))
    }

    def map2[B, C](sb: ScalaState[S, B])(f: (A, B) => C) : ScalaState[S, C] = {
      flatMap(a => sb.flatMap(b => ScalaState.unit(f(a, b))))
    }
  }

  object ScalaState {
    def unit[A, S](a: A) : ScalaState[S, A] = ScalaState(s => (a, s))

    def sequence[S, A](sas : List[ScalaState[S, A]]) : ScalaState[S, List[A]] = {
      sas.reverse.foldLeft(ScalaState.unit[List[A], S](List[A]()))((ra, rla) => ra.map2(rla)((a, b) => b :: a))
    }

    def get[S] : ScalaState[S, S] = ScalaState(s => (s, s))
    def set[S](s: S) : ScalaState[S, Unit] = ScalaState(_ => ((), s))

    def modify[S](f: S => S) : ScalaState[S, Unit] = for {
      s <- get
      _ <- set(f(s))
    } yield()
  }

  sealed trait Input
  case object Coin extends Input
  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int) {}

  object Machine {
    def simulateMachine(inputs: List[Input]): ScalaState[Machine, (Int, Int)] = {
      val states = for {
        input <- inputs
        t <- List(applyInput(input)) // t <- applyInput(input) like applyInput(input).map(t => t) so get (Int, Int) as type for t
      } yield {
        t
      }

      // Now need to pass each machine state to the next scala state in the collection
      // Initial machine is what is passed to the state.run method. Value initialMachine is the initial condition for the system
      ScalaState(initialMachine => {
        val finalMachine = states.foldRight(initialMachine)((currentState, currentMachine) =>  currentState.run(currentMachine)._2)
        ((finalMachine.candies, finalMachine.coins), finalMachine)
      })
    }

    def applyInput(input: Input) : ScalaState[Machine, (Int, Int)] = {
      ScalaState(s => {
        val uMachine =(input, s) match {
          case (_, Machine(_, 0, _)) => s
          case (Coin, Machine(true, cd, cn)) => Machine(false, cd, cn)
          case (Coin, Machine(false, cd, cn)) => Machine(false, cd, cn)
          case (Turn, Machine(true, cd, cn)) => Machine(true, cd, cn)
          case (Turn, Machine(false, cd, cn)) => Machine(true,  cd-1, cn+1)
        }
        ((uMachine.candies, uMachine.coins), uMachine)
      })
    }
  }



  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(42)
    println(ints(3)(rng))
    println(double(rng))

    val rand = unit(3)
    val rand2 = unit(4)
    val test = List(rand, rand2)

    val tes2: Rand[List[Int]] = rng => {(2::(3:: List(): List[Int]), rng)}
    val emptyList: List[Int] = List()
    val nonEmptyList = 3:: emptyList
    println(nonEmptyList)
    println(tes2(rng))
    println(trav(test, rng))
    println(trav(Nil, rng))
    println(sequence(test)(rng))

    val testState = ScalaState[Machine, (Int, Int)](m => ((m.coins + 1, m.candies+1), m))
    println(testState.run(Machine(true, 2,2)))

    val inputs = List(Coin, Turn, Coin, Turn)

    val state = Machine.simulateMachine(inputs)
    println(state.run)
    println(state.run(Machine(true, 5, 3)))
  }


}
