package io.spill.dsl

import scala.language.experimental.macros

trait MetaDslLowPriorityImplicits {
  this: MetaDsl =>

  implicit def materializeQueryMeta[T]: QueryMeta[T] = macro MetaDslMacro.materializeQueryMeta[T]
  implicit def materializeSchemaMeta[T]: SchemaMeta[T] = macro MetaDslMacro.materializeSchemaMeta[T]
}

trait MetaDsl extends MetaDslLowPriorityImplicits {
  this: CoreDsl =>

  type Embedded = io.spill.Embedded

  def schemaMeta[T](
    entity:  String,
    columns: (T => (Any, String))*
  ): SchemaMeta[T] = macro MetaDslMacro.schemaMeta[T]

  def queryMeta[T, R](expand: Quoted[Stream[T] => Stream[R]])(
    extract: R => T
  ): QueryMeta[T] = macro MetaDslMacro.queryMeta[T, R]

  trait SchemaMeta[T] {
    def entity: Quoted[EntityStream[T]]
  }

  trait QueryMeta[T] {
    def expand: Quoted[Stream[T] => Stream[_]]
    def extract: ResultRow => T
  }
}