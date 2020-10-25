package tofu.examples.context.server.app

import cats.{Apply, Defer}
import cats.effect.Sync
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityDecoder, HttpRoutes, Response}
import org.http4s.circe.CirceEntityEncoder._
import org.http4s.circe.CirceEntityDecoder._
import tofu.examples.context.server.{RecipeService, TraceStorage}
import tofu.examples.context.server.model.{Recipe, Trace}
import tofu.generate.GenUUID
import tofu.syntax.context._
import tofu.syntax.monadic._
import tofu.{MonadThrow, WithRun}

object routes {
  def make[F[_]: Sync, G[_]: Apply: WithRun[*[_], F, Trace]](
      recipeService: RecipeService[G],
      traceStorage: TraceStorage[G]
  ): HttpRoutes[F] = {
    val dsl = Http4sDsl[F]
    import dsl._

    HttpRoutes.of[F] {

      case GET -> Root / "requests" => for {
       trace <- Trace.make[F]
        traces <- runContext(traceStorage.storeTrace[Trace] *> traceStorage.all)(trace)
        response <- Ok(traces)
      } yield response

      case GET -> Root / "recipes" / id =>
        for {
          trace     <- Trace.make[F]
          recipe   <- runContext(traceStorage.storeTrace[Trace] *> recipeService.getRecipe(id))(trace)
          response <- recipe.fold(NotFound.apply("No recipe found"))(Ok.apply(_))
        } yield response

      case req @ POST -> Root / "recipes" =>
        for {
          recipe   <- req.as[Recipe]
          trace     <- Trace.make[F]
          id       <- runContext(traceStorage.storeTrace[Trace] *> recipeService.addRecipe(recipe))(trace)
          response <- id.fold(Conflict("Recipe is already present"))(Ok.apply(_))
        } yield response

    }
  }
}
