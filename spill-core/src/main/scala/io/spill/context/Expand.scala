package io.spill.context

import io.spill.ast._
import io.spill.idiom.Statement
import io.spill.idiom.ReifyStatement
import io.spill.NamingStrategy
import io.spill.idiom.Idiom

case class Expand[C <: Context[_, _]](
  val context: C,
  val ast:     Ast,
  statement:   Statement,
  idiom:       Idiom,
  naming:      NamingStrategy
) {

  val (string, externals) =
    ReifyStatement(
      idiom.liftingPlaceholder,
      idiom.emptySetContainsToken,
      statement,
      forProbing = false
    )

  val liftings = externals.collect {
    case lift: ScalarLift => lift
  }

  val prepare =
    (row: context.PrepareRow) => {
      val (_, values, prepare) = liftings.foldLeft((0, List.empty[Any], row)) {
        case ((idx, values, row), lift) =>
          val encoder = lift.encoder.asInstanceOf[context.Encoder[Any]]
          val newRow = encoder(idx, lift.value, row)
          (idx + 1, lift.value :: values, newRow)
      }
      (values, prepare)
    }
}
