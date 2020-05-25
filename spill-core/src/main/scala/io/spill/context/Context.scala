package io.spill.context

import scala.language.higherKinds
import scala.language.experimental.macros
import io.spill.dsl.CoreDsl
import io.spill.util.Messages.fail
import java.io.Closeable

trait Context
  extends Closeable
  with CoreDsl {

  type Result[T]
  type RunQuerySingleResult[T]
  type RunQueryResult[T]
  type RunActionResult
  type RunActionReturningResult[T]
  type RunBatchActionResult
  type RunBatchActionReturningResult[T]
  type Session

  type Prepare = PrepareRow => (List[Any], PrepareRow)
  type Extractor[T] = ResultRow => T

  type StreamResult[E, T]

  // stream or query?
  def stream[E, T](quoted: Quoted[Stream[T]]): StreamResult[E, T] = macro QueryMacro.streamQuery[T]

  // Macro methods do not support default arguments so need to have two methods
  def stream[E, T](quoted: Quoted[Stream[T]], fetchSize: Int): StreamResult[E, T] = macro QueryMacro.streamQueryFetch[T]

  def run[T](quoted: Quoted[T]): Result[RunQuerySingleResult[T]] = macro QueryMacro.runQuerySingle[T]
  def run[T](quoted: Quoted[Stream[T]]): Result[RunQueryResult[T]] = macro QueryMacro.runQuery[T]
  def prepare[T](quoted: Quoted[Stream[T]]): Session => Result[PrepareRow] = macro QueryMacro.prepareQuery[T]

  def run(quoted: Quoted[Action[_]]): Result[RunActionResult] = macro ActionMacro.runAction
  def run[T](
    quoted: Quoted[ActionReturning[_, T]]
  ): Result[RunActionReturningResult[T]] = macro ActionMacro.runActionReturning[T]
  def run(
    quoted: Quoted[BatchAction[Action[_]]]
  ): Result[RunBatchActionResult] = macro ActionMacro.runBatchAction
  def run[T](
    quoted: Quoted[BatchAction[ActionReturning[_, T]]]
  ): Result[RunBatchActionReturningResult[T]] = macro ActionMacro.runBatchActionReturning[T]
  def prepare(quoted: Quoted[Action[_]]): Session => Result[PrepareRow] = macro ActionMacro.prepareAction
  def prepare(
    quoted: Quoted[BatchAction[Action[_]]]
  ): Session => Result[List[PrepareRow]] = macro ActionMacro.prepareBatchAction

  protected val identityPrepare: Prepare = (Nil, _)
  protected val identityExtractor = identity[ResultRow] _

  protected def handleSingleResult[T](list: List[T]) =
    list match {
      case value :: Nil => value
      case other        => fail(s"Expected a single result but got $other")
    }
}
