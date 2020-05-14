package io.spill.norm

import io.spill.ast.Ast
import io.spill.ast.Query
import io.spill.ast.StatelessTransformer
import io.spill.norm.capture.AvoidCapture
import io.spill.ast.Action
import io.spill.util.Interpolator
import io.spill.util.Messages.{TraceType, trace}
import io.spill.util.Messages.TraceType.Normalizations

import scala.annotation.tailrec

object Normalize extends StatelessTransformer {

  val interp = new Interpolator(TraceType.Normalizations, 1)
  import interp._

  override def apply(q: Ast): Ast =
    super.apply(BetaReduction(q))

  override def apply(q: Action): Action =
    NormalizeReturning(super.apply(q))

  override def apply(q: Query): Query =
    trace"Avoid Capture and Normalize" andReturn
      norm(AvoidCapture(q))

  private def traceNorm[T](label: String) =
    trace[T](s"${label} (Normalize)", 1, Normalizations)

  @tailrec
  private def norm(q: Query): Query =
    q match {
      case NormalizeNestedStructures(query) =>
        traceNorm("NormalizeNestedStructures")(query)
        norm(query)
      case ApplyMap(query) =>
        traceNorm("ApplyMap")(query)
        norm(query)
      case SymbolicReduction(query) =>
        traceNorm("SymbolicReduction")(query)
        norm(query)
      case AdHocReduction(query) =>
        traceNorm("AdHocReduction")(query)
        norm(query)
      case OrderTerms(query) =>
        traceNorm("OrderTerms")(query)
        norm(query)
      case NormalizeAggregationIdent(query) =>
        traceNorm("NormalizeAggregationIdent")(query)
        norm(query)
      case other =>
        other
    }
}
