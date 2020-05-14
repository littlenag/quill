package io.spill.quotation

import io.spill.ast.Ast
import io.spill.ast.CollectAst
import io.spill.ast.Dynamic

object IsDynamic {
  def apply(a: Ast) =
    CollectAst(a) { case d: Dynamic => d }.nonEmpty
}
