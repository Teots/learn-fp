package learnfp.applicative

import learnfp.functor.State
import learnfp.functor.StateInstance._

object StateInstance {
  implicit def stateApplicativeInstance[S] = new Applicative[({type E[X] = State[S, X]})#E] {
    override def pure[A](a: A): State[S, A] = State(state => (state, a))
    override def <*>[A, R](fx: State[S, A => R])(a: State[S, A]): State[S, R] = {
      State { state =>
        val (stateFx, valueFx) = fx.run(state)
        val (stateA, valueA) = a.run(stateFx)

        (stateA, valueFx(valueA))
      }
    }
  }

  class StatePureApplicativeOps[A](a:A) {
    def pure[S] = stateApplicativeInstance[S].pure(a)
  }

  implicit def stateToApplicativeOps[S, A, R](a:State[S, A => R]) = new FxApplicativeOps[A, R, ({type E[X] = State[S, X]})#E](a)
  implicit def stateToPureOps[A](a:A) = new StatePureApplicativeOps[A](a)
}
