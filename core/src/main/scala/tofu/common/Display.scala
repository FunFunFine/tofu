package tofu.common
import cats.data.{NonEmptyChain => NEC}
import cats.{Eval, Show}
import derevo.Derivation
import magnolia.{CaseClass, Magnolia, SealedTrait}
import tofu.common.Display.Config

/** Configurable and performant conversion to String */
trait Display[A] extends Show[A] {
  def displayBuild(precedence: Int, cfg: Display.Config, a: A): Eval[NEC[String]]

  def display(a: A, config: Display.Config): String =
    displayBuild(0, config, a).value.toChain.toList.mkString(if (config.useNewline) "\n" else "")

  def show(a: A): String = display(a, Display.Config.default)
}

object Display extends Derivation[Display] with DisplayInstances with DisplaySyntax {

  def apply[A: Display]: Display[A] = implicitly

  private type Typeclass[T] = Display[T]

  def combine[T](ctx: CaseClass[Typeclass, T]): Display[T]    = new Display[T] {
    override def displayBuild(precedence: Int, cfg: Display.Config, a: T): Eval[NEC[String]] = {
      import cfg._
      val shortName: String = ctx.typeName.short
      ctx.parameters
        .foldLeft[Eval[NEC[String]]](
          Eval.now(
            NEC.one(
              s"$shortName${brackets.left}",
            )
          )
        ) { (acc, current) =>
          for {
            alreadyDisplayed   <- acc
            displayedParameter <- current.typeclass.displayBuild(precedence, cfg, current.dereference(a))
            tail                = displayedParameter.tail.initLast.map { case (init, last) =>
                                    init.append(last + fieldSeparator)
                                  }.getOrElse(tail)
            newPart             = NEC
                                    .one(s"${current.label}$fieldAssign${displayedParameter.head}")
                                    .appendChain(tail)
                                    .map(indent + _)
          } yield alreadyDisplayed.concat(newPart)
        }
        .map(_.append(brackets.right))
    }
  }
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Display[T] = ???

  def instance[T]: Display[T] = macro Magnolia.gen[T]

  implicit def generate[T]: Display[T] = macro Magnolia.gen[T]

  final case class Config(
      fieldSeparator: String = ", ",
      indent: String = "\t",
      showFieldNames: Boolean = true,
      brackets: Brackets = Brackets.curly,
      fieldAssign: String = " = ",
      useNewline: Boolean = true
  )

  object Config {

    val default: Config = Config()
  }

  final case class Brackets(left: String, right: String)

  object Brackets {
    val curly: Brackets  = Brackets("{", "}")
    val square: Brackets = Brackets("[", "]")
  }

}

trait DisplayInstances {
  def fromShow[A: Show]: Display[A]                      = (precedence: Int, cfg: Display.Config, a: A) => Eval.now(NEC.one(Show[A].show(a)))
  implicit lazy val intDisplay: Display[Int]             = fromShow(cats.instances.int.catsStdShowForInt)
  implicit lazy val stringDisplay: Display[String]       = (precedence: Int, cfg: Display.Config, a: String) =>
    Eval.now(NEC.one(s""""$a""""))
  implicit lazy val doubleDisplay: Display[Double]       = fromShow(cats.instances.double.catsStdShowForDouble)
  implicit def listDisplay[A: Display]: Display[List[A]] = new Display[List[A]] {
    override def displayBuild(precedence: Int, cfg: Config, a: List[A]): Eval[NEC[String]] =
      Eval.now(
        NEC
          .apply(s"List${cfg.brackets.left}", a.map(Display[A].display(_, cfg)).appended(cfg.brackets.right): _*)
      )
  }

}

trait DisplaySyntax {
  implicit class displayOps[A: Display](a: A) {
    def display(config: Display.Config = Display.Config.default): String = Display[A].display(a, config)
  }

}
