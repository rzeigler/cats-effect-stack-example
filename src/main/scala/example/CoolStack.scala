package example

import cats._
import cats.data._
import cats.implicits._
import cats.mtl._
import cats.mtl.implicits._

import cats.effect.Console.io
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

import shapeless._

import example.Token.Add
import example.Token.Subtract

object CoolStack extends IOApp {

  type Elem    = Token :+: String :+: Int :+: CNil
  type Numeric = String :+: Int :+: CNil

  abstract class Render {
    def render[F[_]: ApplicativeAsk[?[_], Map[String, Int]]: MonadError[?[_], Throwable]]: F[Int]
  }

  val map: Map[String, Int] = Map("a" -> 5, "b" -> 20)

  def run(args: List[String]): IO[ExitCode] = {
    val simple16 = io.putStr("simple stack = ") >> io.putStrLn(
      calculate(
        NonEmptyList.of[Elem](
          Coproduct[Elem](Token.Add: Token),
          Coproduct[Elem](1),
          Coproduct[Elem](Token.Subtract: Token),
          Coproduct[Elem](20),
          Coproduct[Elem](5)
        ),
        map
      ).toString
    )

    val simpleBroken = io.putStr("invalid stack = ") >> io.putStrLn(
      calculate(
        NonEmptyList.of[Elem](Coproduct[Elem](Token.Add: Token)),
        Map.empty
      ).toString
    )

    val withVariables = io.putStr("with variables = ") >> io.putStrLn(
      calculate(
        NonEmptyList.of[Elem](
          Coproduct[Elem](Token.Add: Token),
          Coproduct[Elem](5),
          Coproduct[Elem](Token.Add: Token),
          Coproduct[Elem]("a"),
          Coproduct[Elem]("b")
        ),
        map
      ).toString
    )

    simple16 >> simpleBroken >> withVariables >> IO.pure(ExitCode.Success)
  }

  def calculate(list: NonEmptyList[Elem], vars: Map[String, Int]): Either[Throwable, Int] =
    go[EitherT[RWS[Map[String, Int], Unit, List[Elem], ?], Throwable, ?]].value
      .runA(vars, list.toList.reverse)
      .value

  def go[F[_]: MonadState[?[_], List[Elem]]: ApplicativeAsk[?[_], Map[String, Int]]: MonadError[?[_], Throwable]]
    : F[Int] =
    nextNumeric[F].flatMap(
      right =>
        Stack
          .isEmpty[F]
          .ifM(
            right.pure[F],
            for {
              left <- nextNumeric[F]
              op   <- nextToken
              _ <- Stack.push[F](op match {
                case Token.Add      => Coproduct[Elem](left + right)
                case Token.Subtract => Coproduct[Elem](left - right)
              })
              result <- go
            } yield result
          )
    )

  def nextNumeric[F[_]: MonadState[?[_], List[Elem]]: MonadError[?[_], Throwable]: ApplicativeAsk[
    ?[_],
    Map[String, Int]
  ]]: F[Int] =
    next[F]
      .flatMap(
        n =>
          n.select[String]
            .map(Coproduct[Numeric](_))
            .orElse(n.select[Int].map(Coproduct[Numeric](_)))
            .liftTo[F](new Exception("expected next stack element to be numeric"))
      )
      // Maybe it makes more sense to do this at a highe rlevel, and rduce the number of constraints here, but...
      .flatMap(renderNumeric[F])

  object numericHandler extends Poly1 {
    implicit def string = at[String] { s =>
      new Render {
        def render[F[_]](implicit ask: ApplicativeAsk[F, Map[String, Int]], error: MonadError[F, Throwable]): F[Int] =
          ask.ask.flatMap(_.get(s).liftTo[F](new Exception(s"unknown variable $s")))
      }
    }
    implicit def int = at[Int] { i =>
      new Render {
        def render[F[_]](implicit ask: ApplicativeAsk[F, Map[String, Int]], error: MonadError[F, Throwable]): F[Int] =
          error.point(i)
      }
    }
  }

  def renderNumeric[F[_]: ApplicativeAsk[?[_], Map[String, Int]]: MonadError[?[_], Throwable]](n: Numeric): F[Int] = {
    n.fold(numericHandler)
      .render[F]
  }

  def nextToken[F[_]: MonadState[?[_], List[Elem]]: MonadError[?[_], Throwable]]: F[Token] =
    next[F]
      .flatMap(n => n.select[Token].liftTo[F](new Exception("expected next stack element to be a token")))

  def next[F[_]: MonadState[?[_], List[Elem]]: MonadError[?[_], Throwable]]: F[Elem] =
    Stack.pop[F].flatMap(_.liftTo[F](new Exception("expected non empty stack")))

  object Stack {
    def push[F[_]](elem: Elem)(implicit S: MonadState[F, List[Elem]]): F[Unit] =
      S.modify(elem :: _)

    def pop[F[_]: Monad](implicit S: MonadState[F, List[Elem]]): F[Option[Elem]] =
      peek[F].flatTap(_.traverse_(Function.const(S.modify(_.tail))))

    def peek[F[_]](implicit S: MonadState[F, List[Elem]]): F[Option[Elem]] =
      S.inspect(_.headOption)

    def isEmpty[F[_]](implicit S: MonadState[F, List[Elem]]): F[Boolean] =
      S.inspect(_.isEmpty)
  }
}
