package io.spill.idiom

import scala.reflect.macros.whitebox.Context
import scala.util.Try

import io.spill.NamingStrategy
import io.spill.util.CollectTry
import io.spill.util.LoadObject
import io.spill.CompositeNamingStrategy

object LoadNaming {

  def static(c: Context)(tpe: c.Type): Try[NamingStrategy] =
    CollectTry {
      strategies(c)(tpe).map(LoadObject[NamingStrategy](c)(_))
    }.map(NamingStrategy(_))

  private def strategies(c: Context)(tpe: c.Type) =
    tpe <:< c.typeOf[CompositeNamingStrategy] match {
      case true =>
        tpe.typeArgs
          .filterNot(_ =:= c.weakTypeOf[NamingStrategy])
          .filterNot(_ =:= c.weakTypeOf[scala.Nothing])
      case false =>
        List(tpe)
    }
}
