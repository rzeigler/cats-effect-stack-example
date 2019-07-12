package example

import cats._
import cats.implicits._
import cats.effect.Console.io
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import example.Token.Add
import example.Token.Subtract

object BadStack extends IOApp {
  // Note that I'm using Either as a coproduct (easier than shapeless for simple versions) rather than a MonadError
  type Elem = Either[Token, Int]
  type S    = List[Elem]

  object Stack {
    def push(elem: Elem): MyState[S, Unit] =
      MyState.modify[S](s => (elem :: s)).map(_ => ())

    val pop: MyState[S, Elem] =
      MyState(s => (s.tail, s.head))

    val peek: MyState[S, Elem] = MyState.inspect(_.head)

    val isEmpty: MyState[S, Boolean] =
      MyState(s => (s, s.isEmpty))

    val empty: MyState[S, Unit] = MyState.set[S](List.empty)
  }

  def calculate(list: List[Elem]): Int = {
    go.run(list.reverse)
  }

  val popNextToken = Stack.pop.map(_.fold(identity, _ => sys.error("bad stack")))
  val popNextInt = Stack.pop.map(_.fold(_ => sys.error("bad stack"), identity))

  def go: MyState[S, Int] =
    Stack.pop.flatMap({
      case Left(value) => ???
      case Right(value) =>
        Stack.isEmpty.flatMap(
          isEmpty =>
            // We are done, because this was the last value
            if (isEmpty) MyState.point(value)
            else {
              // Equivalent to below
              // popNextInt.flatMap { next =>
              //   popNextToken.flatMap { op => 
              //       Stack.push(Right(op match {
              //         case Add      => next + value
              //         case Subtract => next - value
              //       })).flatMap { _ =>
              //         go
              //       }
              //   }
              // }
              for {
                next <- popNextInt
                op   <- popNextToken
                _ <- Stack.push(Right(op match {
                  case Add      => next + value
                  case Subtract => next - value
                }))
                value <- go
              } yield value
            }
        )
    })

  def run(args: List[String]): IO[ExitCode] =
    io.putStrLn(calculate(List[Elem](Token.Add.asLeft, 1.asRight, Token.Subtract.asLeft, 20.asRight, 5.asRight))) >>
      io.putStrLn(calculate(List[Elem](Token.Add.asLeft, 5.asRight, Token.Add.asLeft, 20.asRight, 5.asRight))) >>
      // This is a bad stack configuration
      io.putStrLn(
        calculate(List[Elem](Token.Add.asLeft, Token.Add.asLeft, 5.asRight, Token.Add.asLeft, 20.asRight, 5.asRight))
      ) >>
      IO.pure(ExitCode.Error)

}
