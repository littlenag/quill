package io.spill.norm

import io.spill.ast._

object NormalizeStringConcat extends StatelessTransformer {
  override def apply(ast: Ast): Ast = ast match {
    case BinaryOperation(Constant(""), StringOperator.`+`, b) => apply(b)
    case _                                                    => super.apply(ast)
  }
}
