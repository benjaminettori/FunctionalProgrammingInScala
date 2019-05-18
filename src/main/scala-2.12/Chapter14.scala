object Chapter14 {
  /***
    * Motivation
    *
    * We can allow local mutation of variables
    * as long as that mutation does not leak out of the scope
    * where the mutated variable is defined
    *
    * The following rules should hold in order to ensure that mutations remain
    * locally scoped
    *
    * Rule #1 - If we hold a reference to a mutable object, then nothing can observe use mutating it
    *
    * Rule #2 - A mutable object can never be observed outside of the scope in which it was created.
    */

  /***
    * We can defined a data type that will help us implement the above rules
    */


  object Section2 {
    sealed trait ST[S, A] { self =>

      // run is protected because S represents ability to mutate state
      // we don't want the mutation to escape the scope of the type.
      protected def run(s: S) : (A, S)

      def map[B](f: A => B) : ST[S, B] = new ST[S, B] {
        def run(s: S) : (B, S) = {
          val (a, s1) = self.run(s)
          (f(a), s1)
        }
      }

      def flatMap[B](f: A => ST[S, B]) : ST[S, B] = new ST[S, B] {
        def run(s: S): (B, S) = {
          val (a, s1) = self.run(s)
          f(a).run(s1)
        }
      }
    }

    object ST {
      def apply[S, A](a: => A): ST[S, A] = {
        lazy val memo = a
        new ST[S, A] {
          def run(s: S): (A, S) = (memo, s)
        }
      }
    }

    /***
      * Here is a first example of how to restrict access to mutable state
      * Note the type is sealed to ensure it's inner state is not accessible
      *
      * We never actually use the type S
      * It's a "token" - an authorization to access or mutate the cell
      */

    sealed trait STRef[S, A] {
      // cell is protected so can't be accessed outside
      // trait is sealed
      protected var cell : A
      def read() : ST[S, A] = ST(cell)
      def write(a: A) : ST[S, Unit] = new ST[S, Unit] {
        def run(s: S): (Unit, S) = {
          cell = a
          ((), s)
        }
      }
    }

    object STRef {
      def apply[S, A](a: A) : ST[S, STRef[S, A]] = ST(new STRef[S, A] {
        var cell = a
      })
    }


    // Simple program using STRef
    // Conceptual only
    // Doesn't compile even though ST has map and flatMap defined. Get "withFilter not a member of ST[Nothing, Int]
    // For an explanation
    // https://stackoverflow.com/questions/53694354/for-comprehension-with-tuple-withfilter-is-not-a-member-error

//    for {
//      r1 : STRef[Nothing, Int] <- STRef[Nothing, Int](2)
//      r2 : STRef[Nothing, Int] <- STRef[Nothing, Int](1)
//      x : Int <- r1.read()
//      y : Int <- r2.read()
//      _ <-  r1.write(y+ 1)
//      _ <- r2.write(x+1)
//      a : Int <- r1.read()
//      b : Int <- r2.read()
//    } yield(a, b)

    // Can replace with

    STRef[Nothing, Int](2).flatMap(s => {
      val x = s.read()
      val s1 = STRef[Nothing, Int](1)
      s1.flatMap(s2 => {
        val y = s2.read()
        y.flatMap(vy => {
          x.flatMap(vx => {
            s.write(vy+1)
            s2.write(vx + 1)
          })
        })
      })
      s1
    })


  }
}
