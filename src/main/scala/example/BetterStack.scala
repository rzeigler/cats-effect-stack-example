package example

import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import cats.effect.concurrent._
import cats.effect.Console.io

// Switching over to Cats becuase they already have all this fun stuff
object BetterStack extends IOApp {
  type Elem = Either[Token, Int]
  type S    = List[Elem]

  object Stack {
    def push(elem: Elem): State[S, Unit] =
      State.modify(elem :: _)

    val pop: State[S, Option[Elem]] =
      State({
        case head :: tail => tail -> head.some
        case _ => List.empty -> none
      })

    val peek: State[S, Option[Elem]] = 
      State.inspect(_.headOption)

    val isEmpty: State[S, Boolean] = State.inspect(_.isEmpty)

    val empty: State[S, Unit] = State.set(List.empty)
  }

  def calculate(list: NonEmptyList[Elem]): Option[Int] = {
    go.value.runA(list.toList.reverse).value
  }

  def go: OptionT[State[S, ?], Int] =
    OptionT(Stack.pop).flatMap({
      case Left(value) => OptionT.none
      case Right(value) =>
        OptionT
          .liftF(Stack.isEmpty)
          .flatMap(
            isEmpty =>
              if (isEmpty) OptionT.pure(value)
              else
                for {
                  next <- nextInt
                  op   <- nextToken
                  _ <- OptionT.liftF(Stack.push(Right(op match {
                    case Token.Add      => next + value
                    case Token.Subtract => next - value
                  })))
                  value <- go
                } yield value
          )
    })

  def nextInt: OptionT[State[S, ?], Int] =
    OptionT(Stack.pop)
      .flatMap({
        case Left(value)  => OptionT.none
        case Right(value) => OptionT.pure(value)
      })

  def nextToken: OptionT[State[S, ?], Token] =
    OptionT(Stack.pop)
      .flatMap({
        case Left(value)  => OptionT.pure(value)
        case Right(value) => OptionT.none
      })

  def run(args: List[String]): IO[ExitCode] =
    io.putStrLn(calculate(NonEmptyList.of[Elem](Token.Add.asLeft, 1.asRight, Token.Subtract.asLeft, 20.asRight, 5.asRight))) >>
      io.putStrLn(calculate(NonEmptyList.of[Elem](Token.Add.asLeft, 5.asRight, Token.Add.asLeft, 20.asRight, 5.asRight))) >>
      // This is a bad stack configuration
      io.putStrLn(
        calculate(NonEmptyList.of[Elem](Token.Add.asLeft, Token.Add.asLeft, 5.asRight, Token.Add.asLeft, 20.asRight, 5.asRight))
      ) >>
      IO.pure(ExitCode.Error)

}
