package io.spill.dsl

import scala.language.experimental.macros
import io.spill.quotation.NonQuotedException

import scala.annotation.compileTimeOnly

private[spill] trait QueryDsl {
  dsl: CoreDsl =>

  def stream[E, T]: EntityStream[E, T] = macro StreamDslMacro.expandEntity[T]

  object extras extends LowPriorityExtras {
    implicit class NumericOptionOps[A: Numeric](a: Option[A]) {
      def ===[B: Numeric](b: Option[B]): Boolean =
        a.exists(av => b.exists(bv => av == bv))
      def ===[B: Numeric](b: B): Boolean = a.exists(av => av == b)
      def =!=[B: Numeric](b: Option[B]): Boolean =
        a.exists(av => b.exists(bv => av != bv))
      def =!=[B: Numeric](b: B): Boolean = a.exists(av => av != b)
    }
    implicit class NumericRegOps[A: Numeric](a: A) {
      def ===[B: Numeric](b: Option[B]): Boolean = b.exists(bv => bv == a)
      def ===[B: Numeric](b: B): Boolean = a == b
      def =!=[B: Numeric](b: Option[B]): Boolean = b.exists(bv => bv != a)
      def =!=[B: Numeric](b: B): Boolean = a != b
    }
  }

  trait LowPriorityExtras {
    implicit class OptionOps[T](a: Option[T]) {
      def ===(b: Option[T]): Boolean = a.exists(av => b.exists(bv => av == bv))
      def ===(b: T): Boolean = a.exists(av => av == b)
      def =!=(b: Option[T]): Boolean = a.exists(av => b.exists(bv => av != bv))
      def =!=(b: T): Boolean = a.exists(av => av != b)
    }
    implicit class RegOps[T](a: T) {
      def ===(b: Option[T]): Boolean = b.exists(bv => bv == a)
      def ===(b: T): Boolean = a == b
      def =!=(b: Option[T]): Boolean = b.exists(bv => bv != a)
      def =!=(b: T): Boolean = a != b
    }
  }

  // E = Error Channel, which our streams must have
  sealed trait Stream[+E, +T] {

    def map[R](f: T => R): Stream[E, R]

    def withFilter(f: T => Boolean): Stream[E, T]
    def filter(f: T => Boolean): Stream[E, T]

  }

  // Instantiated by StreamDslMacro#expandEntity
  sealed trait EntityStream[E, T] extends Stream[E, T] {

    override def withFilter(f: T => Boolean): EntityStream[E, T]
    override def filter(f: T => Boolean): EntityStream[E, T]
    override def map[R](f: T => R): EntityStream[E, R]
  }
}
