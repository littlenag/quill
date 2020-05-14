package io.spill.context

import io.spill.NamingStrategy
import scala.language.higherKinds
import scala.language.experimental.macros

trait StreamingContext[Idiom <: io.spill.idiom.Idiom, Naming <: NamingStrategy] {
  this: Context[Idiom, Naming] =>

  type StreamResult[T]

  def stream[T](quoted: Quoted[Query[T]]): StreamResult[T] = macro QueryMacro.streamQuery[T]

  // Macro methods do not support default arguments so need to have two methods
  def stream[T](quoted: Quoted[Query[T]], fetchSize: Int): StreamResult[T] = macro QueryMacro.streamQueryFetch[T]
}
