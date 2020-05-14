package io.spill.context.mirror

import scala.reflect.ClassTag

import io.spill.util.Messages.fail

case class Row(data: Any*) {
  def add(value: Any) = Row((data :+ value): _*)
  def apply[T](index: Int)(implicit t: ClassTag[T]) =
    data(index) match {
      case v: T => v
      case other =>
        fail(
          s"Invalid column type. Expected '${t.runtimeClass}', but got '$other'"
        )
    }
}
