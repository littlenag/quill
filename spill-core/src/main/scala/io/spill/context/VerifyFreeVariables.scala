package io.spill.context

import scala.reflect.macros.whitebox.{Context => MacroContext}
import io.spill.quotation.FreeVariables
import io.spill.ast.Ast
import io.spill.util.MacroContextExt._

object VerifyFreeVariables {

  def apply(c: MacroContext)(ast: Ast): Ast =
    FreeVariables(ast) match {
      case free if free.isEmpty => ast
      case free =>
        c.fail(s"""
          |Found the following free variables: ${free.mkString(", ")}.
          |Quotations can't reference values outside their scope directly.
          |In order to bind runtime values to a quotation, please use the method `lift`.
          |Example: `def byName(n: String) = quote(query[Person].filter(_.name == lift(n)))`
        """.stripMargin)
    }
}
