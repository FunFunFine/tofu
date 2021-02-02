package tofu.data.derived

import cats.Eval
import derevo.Derivation
import magnolia.{CaseClass, Magnolia, SealedTrait}
import tofu.common.Display

object display extends Derivation[Display] {

  private type Typeclass[T] = Display[T]

  def combine[T](ctx: CaseClass[Typeclass, T]): Display[T]    = new Display[T] {
    override def displayBuild(precedence: Int, cfg: Display.Config, a: T): Eval[Vector[String]] = {
      import cfg.{fieldSeparator, indent, brackets, fieldAssign}
      val shortName: String = ctx.typeName.short
      ctx.parameters.zipWithIndex
        .foldLeft[Eval[Vector[String]]](
          Eval.now(
            Vector(
              s"$shortName${brackets.left}${cfg.newline}",
            )
          )
        ) { case (acc, (current, index)) =>
          for {
            alreadyDisplayed            <- acc
            displayedParameterValue     <- current.typeclass.displayBuild(precedence, cfg, current.dereference(a))
            //this has at least one element by construction
            adaptedLabeledParameterValue = displayedParameterValue match {
                                             case value +: rest if rest.isEmpty  =>
                                               Vector(indent + current.label + fieldAssign + value)
                                             case typeHeader +: innerValueParams =>
                                               val firstLine   = indent + current.label + fieldAssign + typeHeader
                                               val restOfLines = innerValueParams.map(p => indent + p)
                                               firstLine +: restOfLines
                                             case _                              => Vector(current.label + fieldAssign)
                                           }

            separator                                 = if (index + 1 < ctx.parameters.size) fieldSeparator else ""
            adaptedLabeledParameterValueWithSeparator = adaptedLabeledParameterValue.last + separator + cfg.newline
            separatedLabelValue                       = adaptedLabeledParameterValue.dropRight(1) :+ adaptedLabeledParameterValueWithSeparator
          } yield alreadyDisplayed :++ separatedLabelValue
        }
        .map(s => s :+ brackets.right)
    }
  }
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Display[T] = ???

  def instance[T]: Display[T] = macro Magnolia.gen[T]

}
