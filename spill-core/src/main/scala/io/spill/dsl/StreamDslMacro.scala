package io.spill.dsl

import io.spill.util.MacroContextExt._
import scala.reflect.macros.blackbox.{ Context => MacroContext }

class StreamDslMacro(val c: MacroContext) {

  import c.universe._

  def expandEntity[T](implicit t: WeakTypeTag[T]): Tree =
    q"${meta[T]("Schema")}.entity"

  private def meta[T](prefix: String)(implicit t: WeakTypeTag[T]): Tree = {
    val expanderTpe = c.typecheck(
      tq"io.spill.dsl.MetaDsl#${TypeName(s"${prefix}Meta")}[$t]",
      c.TYPEmode
    )
    c.inferImplicitValue(expanderTpe.tpe, silent = true) match {
      case EmptyTree =>
        c.fail(s"Can't find an implicit `${prefix}Meta` for type `${t.tpe}`")
      case tree => tree
    }
  }
}
