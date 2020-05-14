package io.spill.norm.capture

import io.spill.ast.Query

object AvoidCapture {

  def apply(q: Query): Query =
    Dealias(AvoidAliasConflict(q))
}
