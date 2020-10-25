package tofu.examples.context.server

import cats.{Functor, Monad, Show}
import cats.syntax.show._
import tofu.HasContext
import tofu.concurrent.{Atom, MakeAtom}
import tofu.syntax.context.context
import tofu.syntax.monadic._

class TraceStorage[F[_]: Monad](private val underlying: Atom[F, List[String]]) {
  def storeTrace[A: Show](implicit hasTrace: F HasContext A): F[Unit] = for {
    trace <- context[F]
    _     <- underlying.update(trace.show :: _)
  } yield ()

  def all: F[List[String]] = underlying.get
}

object TraceStorage {
  def make[I[_]: Functor, F[_]: Monad](implicit makeAtom: MakeAtom[I, F]): I[TraceStorage[F]] =
    makeAtom.atom(List.empty[String]).map(new TraceStorage[F](_))
}
