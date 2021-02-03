package tofu.data.derived

import cats.Eval
import derevo.Derivation
import magnolia.{CaseClass, Magnolia, SealedTrait}
import tofu.common.Display

/** Derivation of [[Display]] typeclass for case classes and sealed traits
  *
  * @note Derived [[Display]] instances will indent nested structures if those are supposed to be on newline.
  * You can see examples in the tests.
  */
object display extends Derivation[Display] {

  private type Typeclass[T] = Display[T]

  def combine[T](ctx: CaseClass[Typeclass, T]): Display[T]    = (precedence: Int, cfg: Display.Config, a: T) => {
    import cfg.{fieldSeparator, indent, brackets, fieldAssign, newline}

    def indentIfOnNewline(labeledValue: String) =
      if (labeledValue.startsWith(newline))
        newline + indent + labeledValue.drop(newline.length)
      else labeledValue

    def adaptDisplayedParameter(label: String, displayedParameterValue: Vector[String]): Vector[String] = {
      displayedParameterValue match {
        case value +: rest if rest.isEmpty  =>
          Vector(newline + indent + label + value)
        case typeHeader +: innerValueParams =>
          val labeledTypeHeader =
            newline + indent + label + typeHeader
          val restOfLines       = innerValueParams.map(indentIfOnNewline)
          labeledTypeHeader +: restOfLines
        case _                              => Vector(newline + indent + label)
      }
    }

    val shortName: String = ctx.typeName.short

    ctx.parameters.zipWithIndex
      .foldLeft(
        Eval.now(
          Vector(
            s"$shortName${brackets.left}",
          )
        )
      ) { case (acc, (current, index)) =>
        for {
          alreadyDisplayed                         <- acc
          label                                     = if (cfg.showFieldLabels) current.label + fieldAssign else ""
          displayedParameterValue                  <- current.typeclass.displayBuild(precedence, cfg, current.dereference(a))
          //this value has at least one element in it by construction, but we avoid using NEVector here due to performance and simplicity
          adaptedLabeledParameterValue              = adaptDisplayedParameter(label, displayedParameterValue)
          separator                                 = if (index + 1 < ctx.parameters.size) fieldSeparator else ""
          adaptedLabeledParameterValueWithSeparator = adaptedLabeledParameterValue.last + separator
          separatedLabelValue                       = adaptedLabeledParameterValue.dropRight(1) :+ adaptedLabeledParameterValueWithSeparator
        } yield alreadyDisplayed.appendedAll(separatedLabelValue)
      }
      .map(s => s :+ (newline + brackets.right))
  }
  def dispatch[T](ctx: SealedTrait[Typeclass, T]): Display[T] = (precedence: Int, cfg: Display.Config, a: T) =>
    ctx.dispatch(a)(adtCase => adtCase.typeclass.displayBuild(precedence, cfg, adtCase.cast(a)))

  def instance[T]: Display[T] = macro Magnolia.gen[T]

}
