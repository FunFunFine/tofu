package tofu.examples.context.server
import java.util.UUID

import cats.{Defer, Functor, Monad, Show}
import cats.effect.{ConcurrentEffect, ContextShift, Resource, Sync, Timer}
import cats.syntax.show._
import cats.instances.uuid._
import derevo.derive
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.CirceEntityDecoder._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.{Router, Server}
import org.http4s.{EntityDecoder, HttpApp, HttpRoutes}
import tofu.concurrent.{Atom, MakeAtom}
import tofu.examples.context.server.app.routes
import tofu.{Context, HasContext, MonadThrow, WithContext, WithRun}
import tofu.examples.context.server.model._
import tofu.generate.GenUUID
import tofu.lift.Lift
import tofu.logging.{Loggable, LoggableContext, Logs, ServiceLogging}
import tofu.logging.derivation.{loggable, show}
import tofu.syntax.monadic._
import zio.internal.Platform
import zio.interop.catz._
import zio.interop.catz.implicits._
import zio.{ExitCode, IO, RIO, Task, UIO, URIO}
import tofu.syntax.context._
import tofu.zioInstances.implicits._

import scala.::
import scala.concurrent.ExecutionContext.global

object WebServer extends CatsApp {

  override val platform: Platform =
    Platform.default.withReportFailure(_ => ())

  def makeApp[I[_]: Sync: Lift[*[_], F], F[_]: Sync: WithRun[*[_], I, Trace]]: I[HttpApp[I]] = {
    val validator: RecipeValidate[F] = RecipeValidate.make[F]
    implicit val logsCtx: Logs[I, F] = Logs.withContext[I, F](implicitly, implicitly, Trace.loggableContext[F])

    for {
      recipeTraceStorage <- TraceStorage.make[I, F]
      recipeRepository <- RecipeRepository.make[I, F]
      recipeService     = RecipeService.make[F](validator, recipeRepository)
      recipeRoutes      = routes.make[I, F](recipeService, recipeTraceStorage)
    } yield Router("/" -> recipeRoutes).orNotFound
  }

  def run(args: List[String]): URIO[zio.ZEnv, ExitCode] = {
    makeApp[Task, RIO[Trace, *]]
      .flatMap(app => makeServer[Task](app).use(_ => Task.never))
      .catchAllCause(_ => UIO.unit)
      .as(zio.ExitCode.success)
  }

  private def makeServer[F[_]: ConcurrentEffect: Timer: ContextShift](app: HttpApp[F]): Resource[F, Server[F]] = {
    BlazeServerBuilder[F](global)
      .bindHttp(8080, "0.0.0.0")
      .withHttpApp(app)
      .resource
  }
}




