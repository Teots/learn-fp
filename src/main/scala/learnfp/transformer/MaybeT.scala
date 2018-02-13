package learnfp.transformer

import learnfp.functor.Functor
import learnfp.functor.Maybe.{Just, Maybe, Nothing}
import learnfp.monad.{Monad, MonadOps}
import learnfp.functor.FunctorOps._
import learnfp.monad.MonadOps._

case class MaybeT[A, M[_]](runMaybeT:M[Maybe[A]])(implicit f:Functor[M], m:Monad[M])

object MaybeT {
  implicit def maybeTFunctorInstance[M[_]](implicit f:Functor[M], m:Monad[M]) = new Functor[({type E[X] = MaybeT[X, M]})#E] {
    override def fmap[A, B](a: MaybeT[A, M])(fx: A => B): MaybeT[B, M] = {
      val res: M[Maybe[B]] = f.fmap(a.runMaybeT) { ma =>
        ma match {
          case Just(aValue) => Just(fx(aValue))
          case _ => Nothing()
        }
      }

      MaybeT(res)
    }
  }

  implicit def maybeTMonadInstance[M[_]](implicit f:Functor[M], m:Monad[M]) = new Monad[({type E[X] = MaybeT[X, M]})#E]() {
    override def pure[A](a: A): MaybeT[A, M] = MaybeT(m.pure(Just(a)))
    override def flatMap[A, B](a: MaybeT[A, M])(fx: A => MaybeT[B, M]): MaybeT[B, M] = {
      val res: M[Maybe[B]] = {
        m.flatMap(a.runMaybeT) { aValue =>
          aValue match {
            case Just(am) => fx(am).runMaybeT
            case _ => m.pure(Nothing())
          }
        }
      }

      MaybeT(res)
    }
  }

  implicit def maybeTToMonadOps[A, M[_]](a:MaybeT[A, M])(implicit m:Monad[M], f:Functor[M]) =
    new MonadOps[A, ({type E[X] = MaybeT[X, M]})#E](a)

  implicit def maybeTMonadTransInstance[M[_]](implicit f:Functor[M], m:Monad[M]) = new MonadTransformer[M, MaybeT] {
    override def lift[A](a: M[A]): MaybeT[A, M] = MaybeT(f.fmap(a)(ma => Just(ma)))
  }

  def nothingT[A, M[_]](implicit f:Functor[M], m:Monad[M]):MaybeT[A, M] = MaybeT(m.pure(Nothing()))

  def lift[A, M[_]](a:M[A])(implicit f:Functor[M], m:Monad[M]):MaybeT[A, M] = maybeTMonadTransInstance.lift(a)
}
