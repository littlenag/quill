package io.spill.dsl

import io.spill.quotation.NonQuotedException
import scala.annotation.compileTimeOnly

object UnlimitedTuple {
  @compileTimeOnly(NonQuotedException.message)
  def apply(values: Any*): Nothing = NonQuotedException()
}
