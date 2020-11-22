package tofu.common
import cats.data.Chain
import cats.{Eval, Later, Show}
import derevo.Derivation
import magnolia.{CaseClass, Magnolia, SealedTrait}
import cats.syntax.all._
import cats.instances.list._

/** Configurable and performant conversion to String */
trait Display[A] {
  def displayBuild(precedence: Int, cfg: DisplayConfig, a: A): Eval[Chain[String]]

  def display(a: A): String = displayBuild(0, DisplayConfig.default, a).value.toList.mkString
}

object Display extends Derivation[Display] {

  implicit class displayOps[A: Display](a: A) {
    def display: String = Display[A].display(a)
  }
  implicit def show[A: Display]: Show[A] = _.display

  def apply[A: Display]: Display[A]      = implicitly

  private type Typeclass[T] = Display[T]

  def combine[T](ctx: CaseClass[Typeclass, T]): Display[T] = new Display[T] {
    override def displayBuild(precedence: Int, cfg: DisplayConfig, a: T): Eval[Chain[String]] = Eval.later {
       val typeName: String  = ctx.typeName.toString
       val shortName: String = ctx.typeName.short

      val shownParams = ctx.parameters.map {
        param =>
          param.typeclass.displayBuild(precedence, cfg, param.dereference(a))
      }.toChain

      shownParams.sequence

    }.flatten
  }
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Display[T] = ???

  def instance[T]: Display[T] = macro Magnolia.gen[T]

  implicit def generate[T]: Display[T] = macro Magnolia.gen[T]


}
final case class DisplayConfig(
    fieldSeparator: String = ",\n",
    indent: String = "\t",
    showFieldNames: Boolean = true,
    brackets: (String, String) = "{" -> "}"
)

object DisplayConfig {
  val default: DisplayConfig = DisplayConfig()
}
