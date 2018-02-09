package learnfp.monad

import learnfp.functor.Writer
import learnfp.functor.WriterInstance._
import learnfp.monoid.Monoid
import learnfp.monoid.MonoidOps._

object WriterInstance {
  implicit def writerMonadInstance[W](implicit monoid:Monoid[W]) = new Monad[({type E[X] = Writer[W, X]})#E] {
    override def pure[A](a: A): Writer[W, A] = Writer(() => (monoid.mzero, a))
    override def flatMap[A, B](a: Writer[W, A])(fx: A => Writer[W, B]): Writer[W, B] = {
      val (stateA, outA) = a.run()
      val (stateB, outB) = fx(outA).run()

      Writer(() => (a.monoid.mappend(stateA, stateB), outB))
    }
  }

  implicit def writerToMonadOps[W, A](w:Writer[W, A])(implicit monoid:Monoid[W]) = new MonadOps[A, ({type E[X] = Writer[W, X]})#E](w)
}
