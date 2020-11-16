package tofu.common
import cats.{Show, Eval}

/** Configurable and performant conversion to String */
trait Display[A] {
  def displayBuild(precedence: Int, cfg: DisplayConfig, a: A): Eval[Vector[String]]

  def display(a: A): String = displayBuild(0, DisplayConfig.default, a).value.mkString
}

object Display {
  implicit def show[A: Display]: Show[A] = (a: A) => Display[A].display(a)
  def apply[A: Display]: Display[A] = implicitly
}
final case class DisplayConfig()

object DisplayConfig {
  val default: DisplayConfig = DisplayConfig()
}
