package tofu.common
import cats.{Eval, Show}

/** Configurable and performant conversion to String */
trait Display[A] extends Show[A] {

  /** Represents value as a collection of parts from which it can be shown.
    *
    * @note Newlines are managed solely by instances of [[Display]].
    */
  def displayBuild(cfg: Display.Config, a: A): Eval[Vector[String]]

  def display(a: A, config: Display.Config): String =
    displayBuild(config, a).value.mkString

  def show(a: A): String = display(a, Display.Config.default)
}

object Display extends DisplaySyntax with DisplayInstances {
  def apply[A: Display]: Display[A] = implicitly

  /** @param fieldSeparator
    * @param showFieldLabels
    * @param brackets
    * @param fieldAssign
    * @param newline
    */
  final case class Config(
      fieldSeparator: String = ",",
      indent: String = "\t",
      showFieldLabels: Boolean = true,
      brackets: Brackets = Brackets.curly,
      fieldAssign: String = " = ",
      newline: String = "\n"
  )

  object Config {
    val default: Config = Config()
  }

  final case class Brackets(left: String, right: String)

  object Brackets {
    val curly: Brackets  = Brackets("{", "}")
    val square: Brackets = Brackets("[", "]")
    val round: Brackets  = Brackets("(", ")")
  }

}

trait DisplaySyntax {
  implicit class displayOps[A: Display](a: A) {
    def display(config: Display.Config = Display.Config.default): String = Display[A].display(a, config)
  }

}
trait DisplayInstances {
  def fromShow[A: Show]: Display[A]                       = (cfg: Display.Config, a: A) => Eval.now(Vector(Show[A].show(a)))
  implicit lazy val intDisplay: Display[Int]              = fromShow(cats.instances.int.catsStdShowForInt)
  implicit lazy val stringDisplay: Display[String]        = (cfg: Display.Config, a: String) =>
    Eval.now(Vector("\"" + a + "\""))
  implicit lazy val doubleDisplay: Display[Double]        = fromShow(cats.instances.double.catsStdShowForDouble)
  implicit lazy val longDisplay: Display[Long]            = fromShow(cats.instances.long.catsStdShowForLong)
  implicit lazy val floatDisplay: Display[Float]          = fromShow(cats.instances.float.catsStdShowForFloat)
  implicit def listDisplay[A: Display]: Display[List[A]]  =
    fromShow(cats.instances.list.catsStdShowForList[A])
  implicit def setDisplay[A: Display]: Display[Option[A]] =
    fromShow(cats.instances.option.catsStdShowForOption[A])
  implicit def optionDisplay[A: Display]: Display[Set[A]] =
    fromShow(cats.instances.set.catsStdShowForSet[A])

}
