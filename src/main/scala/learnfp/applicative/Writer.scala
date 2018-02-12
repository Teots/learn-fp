package learnfp.applicative

import learnfp.functor.Writer
import learnfp.functor.WriterInstance._
import learnfp.monoid.Monoid
import learnfp.monoid.MonoidOps._

object WriterInstance {
  implicit def writerApplicativeInstance[W](implicit monoid:Monoid[W]) = new Applicative[({type E[X] = Writer[W, X]})#E] {
    override def pure[A](a: A): Writer[W, A] = Writer(() => (monoid.mzero, a))
    override def <*>[A, R](fx: Writer[W, A => R])(a: Writer[W, A]): Writer[W, R] = {
      val (stateA, valueA) = a.run()
      val (stateFx, valueFx) = fx.run()

      Writer(() => (monoid.mappend(stateFx, stateA), valueFx(valueA)))
    }
  }

  implicit def writerToApplicativeOps[A, R, W](fx:Writer[W, A => R])(implicit monoid:Monoid[W]) =
    new FxApplicativeOps[A, R, ({type E[X] = Writer[W, X]})#E](fx)

  class WriterPureOps[A](a:A) {
    def pure[W](implicit monoid: Monoid[W]) = writerApplicativeInstance[W].pure(a)
  }
  implicit def toPureOps[A](a:A) = new WriterPureOps[A](a)
}
