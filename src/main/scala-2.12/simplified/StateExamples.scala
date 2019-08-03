package simplified

object StateExamples {
  case class GolfState(distance: Int)

  def nextStroke(previousState: GolfState, distanceOfNextHit: Int) : GolfState = GolfState(previousState.distance + distanceOfNextHit)


  // might want to make state a historical record
  case class GolfStateListing(strokes: List[Int])

  def nextStrokes(previousState: GolfStateListing, distanceOfNextHit: Int) : GolfStateListing = GolfStateListing(distanceOfNextHit :: previousState.strokes)

  // Better state model

  object GolfStateMonadic {

    // Start by defining state class as a monad
    // run function allows us to get the values back from the state
    case class State[S, A](run: S => (A, S)) {
      def flatMap[B](f: A => State[S, B]) : State[S, B] = State { startingState: S =>
        val (currentValue, runState) = run(startingState)
        val nextState = f(currentValue)
        nextState.run(runState)
      }

      def map[B](f: A => B) : State[S, B] = this.flatMap { a: A =>
        State.unit(f(a))
      }

    }

    object State {
      // define this in object instead of class so that it can be parametrized any way we want using S, A
      def unit[S, A](a: => A) : State[S, A] = State(run = s => (a, s))
    }

    case class GolfState(distance: Int)

    def swing(distance: Int) : State[GolfState, Int] = State { gs: GolfState =>
      val newDistance = gs.distance + distance
      (newDistance, GolfState(newDistance))
    }
  }

  def main(args: Array[String]) = {
    val state1 = GolfState(10)
    val state2 = nextStroke(state1, 20)

    println(state2)

    val beginningState = GolfStateMonadic.GolfState(10)

    val stateWithNewDistance : GolfStateMonadic.State[GolfStateMonadic.GolfState, Int] = for {
      _ <- GolfStateMonadic.swing(20)
      totalDistance <- GolfStateMonadic.swing(30)
    } yield totalDistance

    val result = stateWithNewDistance.run(beginningState)
    println(result._1)
  }
}
