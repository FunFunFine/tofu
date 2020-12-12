package tofu.common
import cats.data.NonEmptyChain
import cats.{Eval, Show}
import derevo.Derivation
import magnolia.{CaseClass, Magnolia, SealedTrait}
import tofu.common.Display.Config

/** Configurable and performant conversion to String */
trait Display[A] extends Show[A] {
  def displayBuild(precedence: Int, cfg: Display.Config, a: A): Eval[NonEmptyChain[String]]

  def display(a: A): String = displayBuild(0, Display.Config.default, a).value.toChain.toList.mkString("|")

  def show(a: A): String = display(a)
}

object Display extends Derivation[Display] with DisplayInstances with DisplaySyntax {


  def apply[A: Display]: Display[A] = implicitly

  private type Typeclass[T] = Display[T]

  def combine[T](ctx: CaseClass[Typeclass, T]): Display[T]    = new Display[T] {
    override def displayBuild(precedence: Int, cfg: Display.Config, a: T): Eval[NonEmptyChain[String]] = {
      import cfg._
      val shortName: String = ctx.typeName.short

      val dp = ctx.parameters.foldLeft[Eval[NonEmptyChain[String]]](
        Eval.now(
          NonEmptyChain.one(
            s"$shortName${brackets.left}\n",
          )
        )
      ) { (acc, param) =>
        for {
          displayBuildParam <- param.typeclass.displayBuild(precedence, cfg, param.dereference(a))
          cs                <- acc
          result             = cs.append(s"$indent${param.label}$fieldAssign${displayBuildParam.head}")
                                 .appendChain(displayBuildParam.tail).map(e => s"$indent$e").append(fieldSeparator)
        } yield result
      }

      dp.map(_.append(brackets.right))
    }
  }
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Display[T] = ???

  def instance[T]: Display[T] = macro Magnolia.gen[T]

  implicit def generate[T]: Display[T] = macro Magnolia.gen[T]

  final case class Config(
      fieldSeparator: String = ",\n",
      indent: String = "\t",
      showFieldNames: Boolean = true,
      brackets: Brackets = Brackets.curly,
      fieldAssign: String = " = "
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
  def fromShow[A: Show]: Display[A]                      = (precedence: Int, cfg: Display.Config, a: A) =>
    Eval.now(NonEmptyChain.one(Show[A].show(a)))
  implicit lazy val intDisplay: Display[Int]             = fromShow(cats.instances.int.catsStdShowForInt)
  implicit lazy val stringDisplay: Display[String]       = (precedence: Int, cfg: Display.Config, a: String) =>
    Eval.now(NonEmptyChain.one(s""""$a""""))
  implicit lazy val doubleDisplay: Display[Double]       = fromShow(cats.instances.double.catsStdShowForDouble)
  implicit def listDisplay[A: Display]: Display[List[A]] = new Display[List[A]] {
    override def displayBuild(precedence: Int, cfg: Config, a: List[A]): Eval[NonEmptyChain[String]] =
      Eval.now(NonEmptyChain.apply(s"List${cfg.brackets.left}", a.map(Display[A].display).appended(cfg.brackets.right): _*))
  }

}

trait DisplaySyntax {
  implicit class displayOps[A: Display](a: A) {
    def display: String = Display[A].display(a)
  }


}
