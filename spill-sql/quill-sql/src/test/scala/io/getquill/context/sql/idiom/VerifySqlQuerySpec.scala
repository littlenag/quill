package io.getquill.context.sql.idiom

import io.getquill.Spec
import io.getquill.context.sql.testContext._
import io.getquill.context.sql.SqlQuery
import scala.util.Try
import io.getquill.context.sql.norm.SqlNormalize

class VerifySqlQuerySpec extends Spec {

  "fails if the query can't be translated to applicative joins" - {
    "sortBy" in {
      val q = quote {
        qr1.flatMap(a => qr2.filter(b => b.s == a.s).sortBy(b => b.s).map(b => b.s))
      }
      VerifySqlQuery(SqlQuery(q.ast)).toString mustEqual
        "Some(The monad composition can't be expressed using applicative joins. Faulty expression: 'b.s == a.s'. Free variables: 'List(a)'.)"
    }

    "take" in {
      val q = quote {
        qr1.flatMap(a => qr2.filter(b => b.s == a.s).take(10).map(b => b.s))
      }
      VerifySqlQuery(SqlQuery(q.ast)).toString mustEqual
        "Some(The monad composition can't be expressed using applicative joins. Faulty expression: 'b.s == a.s'. Free variables: 'List(a)'.)"
    }

    "accepts table reference" in {
      val q = quote {
        qr1.leftJoin(qr2).on((a, b) => a.i == b.i).filter {
          case (a, b) => b.isDefined
        }
      }
      VerifySqlQuery(SqlQuery(SqlNormalize(q.ast))) mustEqual None
    }

    "invalid flatJoin on" in {
      val q = quote {
        for {
          a <- qr1
          b <- qr2 if a.i == b.i
          c <- qr1.leftJoin(_.i == a.i)
        } yield (a.i, b.i, c.map(_.i))
      }
      Try(VerifySqlQuery(SqlQuery(q.ast))).isFailure mustEqual true
    }

  }
}
