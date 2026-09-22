package essence

import cats.*
import cats.syntax.all.*

import scala.annotation.tailrec

object Interpreter {

  type Name = String

  enum Term {
    case Variable(name: Name) // x
    case Constant(value: Int) // 42
    case Add(left: Term, right: Term) // left + right
    case Lambda(variable: Name, term: Term) // \x -> term(x)
    case Apply(func: Term, arg: Term) // func(value)

    case Count
    case Ambiguous(alt1: Term, alt2: Term)
  }
}

open class Interpreter[M[_]: Monad] {

  import Interpreter.*

  enum Value {
    case Wrong
    case Number(value: Int)
    case Func(func: Value => M[Value])
  }

  private type Environment = List[(Name, Value)]

  def interp(term: Term, environment: Environment = Nil): M[Value] = term match {
    case Term.Variable(name) =>
      lookup(name, environment)
    case Term.Constant(value) =>
      unitM(Value.Number(value))
    case Term.Add(left, right) =>
      for {
        a <- interp(left, environment)
        b <- interp(right, environment)
        s <- add(a, b)
      } yield s
    case Term.Lambda(variable, term) =>
      val func = Value.Func { arg =>
        interp(term, (variable, arg) :: environment)
      }
      unitM(func)
    case Term.Apply(func, arg) =>
      for {
        f <- interp(func, environment)
        a <- interp(arg, environment)
        r <- apply(f, a)
      } yield r
    case Term.Count =>
      for {
        c <- fetch
      } yield Value.Number(c)
    case Term.Ambiguous(alt1, alt2) =>
      ambiguous(interp(alt1, environment), interp(alt2, environment))
  }

  @tailrec
  private def lookup(name: Name, environment: Environment): M[Value] = environment match {
    case Nil         => error(s"Unbound variable: $name")
    case (n, v) :: e => if n == name then unitM(v) else lookup(name, e)
  }

  private def add(left: Value, right: Value): M[Value] = (left, right) match {
    case (Value.Number(left), Value.Number(right)) =>
      for {
        _ <- tick
      } yield Value.Number(left + right)
    case _ => error(s"Should be numbers: $left,$right")
  }

  private def apply(func: Value, arg: Value): M[Value] = func match {
    case Value.Func(func) =>
      for {
        _ <- tick
        r <- func.apply(arg)
      } yield r
    case _ => error(s"Should be function: $func")
  }

  private def unitM[A](v: A): M[A] = Monad[M].pure(v)

  def error(msg: String): M[Value] = unitM(Value.Wrong)

  def tick: M[Unit] = unitM(())

  def fetch: M[Int] = unitM(0)

  def ambiguous(alt1: M[Value], alt2: M[Value]): M[Value] = error("Ambiguous result")
}
