package io.getquill.context.sql.idiom

import io.getquill.Spec
import io.getquill.PipelineDBDialect
import io.getquill.SqlMirrorContext
import io.getquill.Literal
import io.getquill.TestEntities

class PipelineDBDialectSpec extends Spec {

  val ctx = new SqlMirrorContext(PipelineDBDialect, Literal) with TestEntities
  import ctx._

  "applies explicit casts" - {
    "toLong" in {
      val q = quote {
        qr1.map(t => t.s.toLong)
      }
      ctx.run(q).string mustEqual "SELECT t.s::bigint FROM TestEntity t"
    }
    "toInt" in {
      val q = quote {
        qr1.map(t => t.s.toInt)
      }
      ctx.run(q).string mustEqual "SELECT t.s::integer FROM TestEntity t"
    }
  }

  "Array Operations" - {
    case class ArrayOps(id: Int, numbers: Vector[Int])
    "contains" in {
      ctx.run(query[ArrayOps].filter(_.numbers.contains(10))).string mustEqual
        "SELECT x1.id, x1.numbers FROM ArrayOps x1 WHERE 10 = ANY(x1.numbers)"
    }
  }

  "prepareForProbing" in {
    import PipelineDBDialect._
    val id = preparedStatementId.get()

    prepareForProbing("SELECT t.x1, t.x2 FROM tb t WHERE (t.x1 = ?) AND (t.x2 = ?)") mustEqual
      s"PREPARE p${id + 1} AS SELECT t.x1, t.x2 FROM tb t WHERE (t.x1 = $$1) AND (t.x2 = $$2)"

    prepareForProbing("INSERT INTO tb (x1,x2,x3) VALUES (?,?,?)") mustEqual
      s"PREPARE p${id + 2} AS INSERT INTO tb (x1,x2,x3) VALUES ($$1,$$2,$$3)"
  }
}