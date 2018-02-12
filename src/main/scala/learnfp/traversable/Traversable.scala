package learnfp.traversable

import learnfp.applicative.Applicative
import learnfp.foldable.Foldable
import learnfp.functor.{Functor, Id}


trait Traversable[C[_]] {
  def traverse[A, B, F[_]](xs:C[F[A]])(fx:A => B)(implicit foldable: Foldable[C], functor:Functor[F], applicative: Applicative[F]):F[C[B]]
}

object TraversableInstances {
  import learnfp.foldable.FoldableInstances._
  import learnfp.applicative.ApplicativeOps._
  import learnfp.functor.FunctorOps._

  implicit val idTraversableInstance = new Traversable[Id] {
    override def traverse[A, B, F[_]](xs: Id[F[A]])(fx:A => B)(implicit foldable: Foldable[Id], functor:Functor[F], applicative: Applicative[F]): F[Id[B]] = {
      functor.fmap(xs.value)(a => Id(fx(a)))
    }
  }

  type STuple3[A] = (A, A, A)
  def stuple3[A](a:A, b:A, c:A):STuple3[A] = (a, b, c)

  implicit val tuple3TraversableInstance = new Traversable[STuple3] {
    override def traverse[A, B, F[_]](xs: (F[A], F[A], F[A]))(fx: A => B)(implicit foldable: Foldable[STuple3],
                                                                          functor: Functor[F], applicative: Applicative[F]): F[(B, B, B)] = {

      val aV: F[B] = functor.fmap(xs._3)(a => fx(a))
      val aF: F[B => (B, B)] = functor.fmap(xs._2)((a: A) => (b: B) => (fx(a), b))
      val bV: F[(B, B)] = applicative.<*>(aF)(aV)
      val bF: F[((B, B)) => (B, B, B)] = functor.fmap(xs._1)((a: A) => (b: (B, B)) => (fx(a), b._1, b._2))
      val cV: F[(B,B,B)] = applicative.<*>(bF)(bV)

      cV
    }
  }

  implicit val listTraversableInstance = new Traversable[List] {
    override def traverse[A, B, F[_]](xs: List[F[A]])(fx: A => B)(implicit foldable: Foldable[List],
                                                                  functor: Functor[F], applicative: Applicative[F]): F[List[B]] = {
      foldable.foldr(xs)(applicative.pure(List.empty[B])) { (elem, acc) =>
        val aV: F[B] = functor.fmap(elem)(fx)
        val af: (B => List[B] => List[B]) = (b: B) => (bs: List[B]) => b :: bs
        val bf: F[List[B] => List[B]] = functor.fmap(aV)(af)

        val res: F[List[B]] = applicative.<*>(bf)(acc)

        res
      }
    }
  }
}

class TraversableOps[A, C[_], F[_]](xs:C[F[A]])(implicit foldable: Foldable[C], traversable: Traversable[C],
                                                functor: Functor[F], applicative: Applicative[F]) {
  def traverse[B](fx:A=>B):F[C[B]] = traversable.traverse(xs)(fx)
  def sequence:F[C[A]] = traversable.traverse(xs)(identity)
}

object TraversableOps {
  implicit def toTraversableOps[A, C[_], F[_]](xs:C[F[A]])(implicit functor:Functor[F], applicative: Applicative[F],
                                                           foldable:Foldable[C], traversable: Traversable[C]) = new TraversableOps[A, C, F](xs)
}
