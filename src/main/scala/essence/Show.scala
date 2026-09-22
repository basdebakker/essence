package essence

import cats.data.*
import cats.implicits.*
import cats.{Show as _, *}

trait Show[F[_]] {
  def show[A](value: F[A]): String
}

object Show {

  def show[F[_]: Show, A](value: F[A]): String =
    summon[Show[F]].show(value)

  given Show[Id] {
    def show[A](value: Id[A]): String =
      value.toString
  }

  type Error[A] = Either[String, A]
  given Show[Error] {
    def show[A](value: Either[String, A]): String =
      value.fold(
        l => s"Failure: $l",
        r => s"Success: $r"
      )
  }

  given Show[Seq] {
    def show[A](value: Seq[A]): String =
      value.mkString("\n")
  }

  given Show[Eval] {
    def show[A](value: Eval[A]): String =
      value.value.toString
  }

  given [M[_]: {FlatMap, Show}] => Show[[A] =>> StateT[M, Int, A]] {
    def show[A](value: StateT[M, Int, A]): String = {
      val msg: M[String] =
        for {
          (finalState, finalValue) <- value.run(0)
        } yield s"count = $finalState, value = $finalValue"
      Show.show(msg)
    }
  }

  given [M[_]: {Functor, Show}] => Show[[A] =>> EitherT[M, String, A]] {
    def show[A](value: EitherT[M, String, A]): String = {
      val msg: M[String] = value.fold(
        l => s"Failure: $l",
        r => s"Success: $r"
      )
      Show.show(msg)
    }
  }

  given [M[_]: {Monad, Show}] => Show[[A] =>> SeqT[M, A]] {
    def show[A](value: SeqT[M, A]): String = {
      val msg: M[String] = value.mkString("\n")
      Show.show(msg)
    }
  }
}
