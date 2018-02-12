package learnfp.applicative

import learnfp.functor.Disjunction._
import learnfp.functor.DisjunctionInstance._

object DisjunctionInstance {
  implicit def disjunctionInstance[L] = new Applicative[({type E[X] = Disjunction[L, X]})#E]() {
    override def pure[A](a: A): Disjunction[L, A] = RightDisjunction(a)
    override def <*>[A, R](dfx: Disjunction[L, A => R])(da: Disjunction[L, A]): Disjunction[L, R] = {
      (da, dfx) match {
        case (RightDisjunction(daValue), RightDisjunction(dfxValue)) => RightDisjunction(dfxValue(daValue))
        case (LeftDisjunction(daFailure), RightDisjunction(_)) => LeftDisjunction(daFailure)
        case (RightDisjunction(_), LeftDisjunction(dfxFailure)) => LeftDisjunction(dfxFailure)
        case (LeftDisjunction(_), LeftDisjunction(dfxFailure)) => LeftDisjunction(dfxFailure)
      }
    }
  }

  implicit def disjunctionToApplicativeOps[L, A, R](fx:Disjunction[L, A => R])(
    implicit applicative:Applicative[({type E[X] = Disjunction[L, X]})#E]) =
    new FxApplicativeOps[A, R, ({type E[X] = Disjunction[L, X]})#E](fx)
}
