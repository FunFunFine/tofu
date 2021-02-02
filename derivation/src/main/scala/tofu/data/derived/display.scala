package tofu.data.derived

import cats.Eval
import derevo.Derivation
import magnolia.{CaseClass, Magnolia, SealedTrait}
import cats.data.{NonEmptyChain => NEC}
import cats.{Eval, Show}
import derevo.Derivation
import tofu.common.Display
import tofu.common.Display.Config

object display extends Derivation[Display] {

  private type Typeclass[T] = Display[T]

  def combine[T](ctx: CaseClass[Typeclass, T]): Display[T]    = new Display[T] {
    override def displayBuild(precedence: Int, cfg: Display.Config, a: T): Eval[Vector[String]] = {
      import cfg._
      val shortName: String = ctx.typeName.short
      ctx.parameters
        .foldLeft[Eval[Vector[String]]](
          Eval.now(
            Vector(
              s"$shortName${brackets.left}",
            )
          )
        ) { (acc, current) =>
          for {
            alreadyDisplayed        <- acc
            displayedParameterValue <- current.typeclass.displayBuild(precedence, cfg, current.dereference(a))
            //this has at least one element by construction
            labelValue               = displayedParameterValue match {
                                         // Vector("Foo{", "value = 3", ...
                                         case typeHead +: typeParams =>
                                           val firstLine   = current.label + fieldAssign + typeHead
                                           val restOfLines = typeParams.map(indent + _)
                                           firstLine +: restOfLines
                                         case _                      => Vector(current.label + fieldAssign)
                                       }

            lastElementWithSeparator = labelValue.last + fieldSeparator
            separatedLabelValue      = labelValue.dropRight(1) :+ lastElementWithSeparator
          } yield alreadyDisplayed :++ separatedLabelValue
        }
        .map(s => s :+ brackets.right)
    }
  }
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Display[T] = ???

  def instance[T]: Display[T] = macro Magnolia.gen[T]

}

