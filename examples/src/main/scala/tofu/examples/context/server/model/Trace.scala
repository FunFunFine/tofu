package tofu.examples.context.server.model

import java.util.UUID

import cats.{Functor, Show}
import cats.syntax.show._
import tofu.HasContext
import tofu.generate.GenUUID
import tofu.syntax.monadic._
import tofu.logging.derivation.loggable
import tofu.logging.{Loggable, LoggableContext}

final case class Trace(traceId: UUID)

object Trace {
  def make[F[_]: Functor: GenUUID]: F[Trace] = GenUUID.random[F].map(Trace.apply)
  implicit val recipeTraceShow: Show[Trace]                                     = Show.show(trace => s"Trace(id = ${trace.traceId.show}")
  implicit val traceLoggable: Loggable[Trace]                                   = loggable.byShow("traceId")
  implicit def loggableContext[G[_]: *[_] HasContext Trace]: LoggableContext[G] =
    LoggableContext
      .of[G]
      .instance[Trace]
}
