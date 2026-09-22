package essence

import cats.*
import cats.implicits.*

final case class SeqT[M[_]: Monad, A](value: M[Seq[A]]) {

  def mkString(sep: String): M[String] = summon[Monad[M]].map(value)(_.mkString(sep))
}

object SeqT {

  def liftM[M[_]: Monad, A](ma: M[A]): SeqT[M, A] = SeqT(summon[Monad[M]].map(ma)(Seq(_)))

  given [M[_]: Monad] => Monad[[A] =>> SeqT[M, A]] {

    override def pure[A](a: A): SeqT[M, A] = {
      val ma = summon[Monad[M]].pure(a)
      liftM(ma)
    }

    override def flatMap[A, B](ma: SeqT[M, A])(f: A => SeqT[M, B]): SeqT[M, B] = {
      def g(sa: Seq[A]): M[Seq[B]] = sa.map(a => f(a).value).flatSequence
      SeqT(ma.value.flatMap(g))
    }

    override def tailRecM[A, B](a: A)(f: A => SeqT[M, Either[A, B]]): SeqT[M, B] = ???
  }
}
