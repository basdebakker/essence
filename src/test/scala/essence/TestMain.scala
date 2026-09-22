package essence

import cats.data.*
import cats.implicits.*
import cats.{Show as _, *}
import essence.Interpreter.*
import org.junit.Test

class TestMain {

  @Test
  def testId(): Unit = {
    val interpreter = new Interpreter[Id]
    runTest(interpreter)
  }

  @Test
  def testError(): Unit = {
    type Error[A] = Either[String, A]
    val interpreter = new Interpreter[Error] {
      override def error(msg: Name): Error[Value] = Left(msg)
    }
    runTest(interpreter)
  }

  @Test
  def testState(): Unit = {
    type MyState[A] = State[Int, A]
    val interpreter = new Interpreter[MyState] {
      override def tick: MyState[Unit] = State.modify(_ + 1)
      override def fetch: MyState[Int] = State.get
    }
    runTest(interpreter)
  }

  @Test
  def testErrorAndState(): Unit = {
    type Error[A] = Either[String, A]
    type MyState[A] = StateT[Error, Int, A]
    val interpreter = new Interpreter[MyState] {
      override def error(msg: Name): MyState[Value] = StateT.liftF(Left(msg))
      override def tick: MyState[Unit] = StateT.modify(_ + 1)
      override def fetch: MyState[Int] = StateT.get
    }
    runTest(interpreter)
  }

  @Test
  def testStateAndError(): Unit = {
    type MyState[A] = State[Int, A]
    type Error[A] = EitherT[MyState, String, A]
    val interpreter = new Interpreter[Error] {
      override def error(msg: Name): Error[Value] = EitherT.leftT(msg)
      override def tick: Error[Unit] = EitherT.liftF(State.modify(_ + 1))
      override def fetch: Error[Int] = EitherT.liftF(State.get)
    }
    runTest(interpreter)
  }

  @Test
  def testSeq(): Unit = {
    val interpreter = new Interpreter[Seq] {
      override def ambiguous(alt1: Seq[Value], alt2: Seq[Value]): Seq[Value] = alt1 ++ alt2
    }
    runTest(interpreter)
  }

  @Test
  def testSeqAndError(): Unit = {
    type Error[A] = EitherT[Seq, String, A]
    val interpreter = new Interpreter[Error] {
      override def error(msg: Name): Error[Value] = EitherT.leftT(msg)
      override def ambiguous(alt1: Error[Value], alt2: Error[Value]): Error[Value] =
        EitherT(alt1.value ++ alt2.value)
    }
    runTest(interpreter)
  }

  @Test
  def testErrorAndSeq(): Unit = {
    type Error[A] = Either[String, A]
    type MySeq[A] = SeqT[Error, A]
    val interpreter = new Interpreter[MySeq] {
      override def error(msg: String): MySeq[Value] = SeqT.liftM(Left(msg))
      override def ambiguous(alt1: MySeq[Value], alt2: MySeq[Value]): MySeq[Value] = alt1 ++ alt2
    }
    runTest(interpreter)
  }

  @Test
  def testSeqAndState(): Unit = {
    type MyState[A] = StateT[Seq, Int, A]
    val interpreter = new Interpreter[MyState] {
      override def tick: MyState[Unit] = StateT.modify(_ + 1)
      override def fetch: MyState[Int] = StateT.get
      override def ambiguous(alt1: MyState[Value], alt2: MyState[Value]): MyState[Value] =
        StateT.applyF(alt1.runF ++ alt2.runF)
    }
    runTest(interpreter)
  }

  @Test
  def testStateAndSeq(): Unit = {
    type MyState[A] = State[Int, A]
    type MySeq[A] = SeqT[MyState, A]
    val interpreter = new Interpreter[MySeq] {
      override def tick: MySeq[Unit] = SeqT.liftM(State.modify(_ + 1))
      override def fetch: MySeq[Int] = SeqT.liftM(State.get)
      override def ambiguous(alt1: MySeq[Value], alt2: MySeq[Value]): MySeq[Value] = alt1 ++ alt2
    }
    runTest(interpreter)
  }

  @Test
  def testSeqAndStateAndError(): Unit = {
    type MyState[A] = StateT[Seq, Int, A]
    type Error[A] = EitherT[MyState, String, A]
    val interpreter = new Interpreter[Error] {
      override def error(msg: String): Error[Value] = EitherT.leftT(msg)
      override def tick: Error[Unit] = EitherT.liftF(StateT.modify(_ + 1))
      override def fetch: Error[Int] = EitherT.liftF(StateT.get)
      override def ambiguous(alt1: Error[Value], alt2: Error[Value]): Error[Value] =
        EitherT(StateT.applyF(alt1.value.runF ++ alt2.value.runF))
    }
    runTest(interpreter)
  }

  @Test
  def testSeqAndErrorAndState(): Unit = {
    type Error[A] = EitherT[Seq, String, A]
    type MyState[A] = StateT[Error, Int, A]
    val interpreter = new Interpreter[MyState] {
      override def error(msg: String): MyState[Value] = StateT.liftF(EitherT.leftT(msg))
      override def tick: MyState[Unit] = StateT.modify(_ + 1)
      override def fetch: MyState[Int] = StateT.get
      override def ambiguous(alt1: MyState[Value], alt2: MyState[Value]): MyState[Value] =
        StateT.applyF(EitherT(alt1.runF.value ++ alt2.runF.value))
    }
    runTest(interpreter)
  }

  @Test
  def testStateAndSeqAndError(): Unit = {
    type MyState[A] = State[Int, A]
    type MySeq[A] = SeqT[MyState, A]
    type Error[A] = EitherT[MySeq, String, A]
    val interpreter = new Interpreter[Error] {
      override def error(msg: String): Error[Value] = EitherT.leftT(msg)
      override def tick: Error[Unit] = EitherT.liftF(SeqT.liftM(State.modify(_ + 1)))
      override def fetch: Error[Int] = EitherT.liftF(SeqT.liftM(State.get))
      override def ambiguous(alt1: Error[Value], alt2: Error[Value]): Error[Value] =
        EitherT(alt1.value ++ alt2.value)
    }
    runTest(interpreter)
  }

  @Test
  def testStateAndErrorAndSeq(): Unit = {
    type MyState[A] = State[Int, A]
    type Error[A] = EitherT[MyState, String, A]
    type MySeq[A] = SeqT[Error, A]
    val interpreter = new Interpreter[MySeq] {
      override def error(msg: String): MySeq[Value] = SeqT.liftM(EitherT.leftT(msg))
      override def tick: MySeq[Unit] = SeqT.liftM(EitherT.liftF(StateT.modify(_ + 1)))
      override def fetch: MySeq[Int] = SeqT.liftM(EitherT.liftF(StateT.get))
      override def ambiguous(alt1: MySeq[Value], alt2: MySeq[Value]): MySeq[Value] =
        alt1 ++ alt2
    }
    runTest(interpreter)
  }

  @Test
  def testErrorAndSeqAndState(): Unit = {
    type Error[A] = Either[String, A]
    type MySeq[A] = SeqT[Error, A]
    type MyState[A] = StateT[MySeq, Int, A]
    val interpreter = new Interpreter[MyState] {
      override def error(msg: String): MyState[Value] = StateT.liftF(SeqT.liftM(Left(msg)))
      override def tick: MyState[Unit] = StateT.modify(_ + 1)
      override def fetch: MyState[Int] = StateT.get
      override def ambiguous(alt1: MyState[Value], alt2: MyState[Value]): MyState[Value] =
        StateT.applyF(alt1.runF ++ alt2.runF)
    }
    runTest(interpreter)
  }

  @Test
  def testErrorAndStateAndSeq(): Unit = {
    type Error[A] = Either[String, A]
    type MyState[A] = StateT[Error, Int, A]
    type MySeq[A] = SeqT[MyState, A]
    val interpreter = new Interpreter[MySeq] {
      override def error(msg: String): MySeq[Value] = SeqT.liftM(StateT.liftF(Left(msg)))
      override def tick: MySeq[Unit] = SeqT.liftM(StateT.modify(_ + 1))
      override def fetch: MySeq[Int] = SeqT.liftM(StateT.get)
      override def ambiguous(alt1: MySeq[Value], alt2: MySeq[Value]): MySeq[Value] =
        alt1 ++ alt2
    }
    runTest(interpreter)
  }

  private def allTerms: Seq[Term] = {
    import Term.*
    Seq(
      Apply(Lambda("x", Add(Variable("x"), Variable("x"))), Add(Constant(10), Constant(11))),
      Apply(Lambda("x", Apply(Variable("x"), Constant(21))), Lambda("y", Add(Variable("y"), Variable("y")))),
      Apply(Constant(1), Add(Constant(2), Constant(3))),
      Add(Add(Constant(1), Constant(2)), Count),
      Apply(Ambiguous(Lambda("x", Variable("x")), Lambda("x", Add(Variable("x"), Variable("x")))), Ambiguous(Constant(1), Constant(3))),
      Apply(Ambiguous(Lambda("x", Add(Variable("x"), Variable("x"))), Constant(2)), Constant(21)),
      Apply(Ambiguous(Lambda("x", Variable("x")), Lambda("x", Add(Variable("x"), Variable("x")))), Ambiguous(Count, Constant(3)))
    )
  }

  private def runTest[M[_]: {Monad, Show}](interpreter: Interpreter[M]): Unit = {
    allTerms.foreach { term =>
      println(term)
      println(prettyPrint(term))
      val result: M[interpreter.Value] = interpreter.interp(term)
      val msg: String = Show.show(result)
      println(msg)
      println()
    }
    println()
  }

  private def prettyPrint(term: Term): String = {
    import Term.*
    term match {
      case Variable(name)         => name
      case Constant(value)        => value.toString
      case Add(left, right)       => s"(${prettyPrint(left)} + ${prettyPrint(right)})"
      case Lambda(variable, term) => s"\\$variable -> ${prettyPrint(term)}"
      case Apply(func, value)     => s"(${prettyPrint(func)})(${prettyPrint(value)})"
      case Count                  => "Count"
      case Ambiguous(alt1, alt2)  => s"${prettyPrint(alt1)} | ${prettyPrint(alt2)}"
    }
  }
}
