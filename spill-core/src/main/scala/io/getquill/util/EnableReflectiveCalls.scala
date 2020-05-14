package io.spill.util

import scala.reflect.macros.blackbox.Context

object EnableReflectiveCalls {

  def apply(c: Context) = {
    import c.universe._
    q"import _root_.scala.language.reflectiveCalls" ::
      q"Nil.asInstanceOf[{ def size }].size" ::
      Nil
  }
}
