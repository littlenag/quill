package test

import io.spill.Spec
import io.spill.ast._
import io.spill.testContext.EntityStream
import io.spill.testContext.InfixInterpolator
import io.spill.testContext.Stream
import io.spill.testContext.TestEntity
import io.spill.testContext.qr1
import io.spill.testContext.stream
import io.spill.testContext.quote
import io.spill.testContext.unquote
import io.spill.testContext.Quoted

class OpsSpec extends Spec {

  "quotes asts" - {
    "explicitly" in {
      val q = quote {
        stream[TestEntity]
      }
      q.ast mustEqual Entity("TestEntity", Nil)
    }
    "implicitly" in {
      val q: Quoted[Stream[TestEntity]] =
        stream[TestEntity]
      q.ast mustEqual Entity("TestEntity", Nil)
    }
  }

  "unquotes asts" - {
    "explicitly" in {
      val q = quote {
        unquote(qr1).map(t => t)
      }
      q.ast mustEqual Map(Entity("TestEntity", Nil), Ident("t"), Ident("t"))
    }
    "implicitly" in {
      val q = quote {
        qr1.map(t => t)
      }
      q.ast mustEqual Map(Entity("TestEntity", Nil), Ident("t"), Ident("t"))
    }
  }

  "provides the infix interpolator" - {
    "with `as`" in {
      val q = quote {
        infix"true".as[Boolean]
      }
      q.ast mustEqual Infix(List("true"), Nil, false)
    }
    "without `as`" in {
      val q = quote {
        infix"true"
      }
      q.ast mustEqual Infix(List("true"), Nil, false)
    }
  }

  "unquotes duble quotations" in {
    val q: Quoted[EntityStream[TestEntity]] = quote {
      quote(stream[TestEntity])
    }
    val n = quote {
      stream[TestEntity]
    }
    q.ast mustEqual n.ast
  }

  implicit class QueryOps[Q <: Stream[_]](q: Q) {
    def allowFiltering = quote(infix"$q ALLOW FILTERING".as[Q])
  }

  "unquotes quoted function bodies automatically" - {
    "one param" in {
      val q: Quoted[Int => EntityStream[TestEntity]] = quote { (i: Int) =>
        stream[TestEntity].allowFiltering
      }
      val n = quote { (i: Int) =>
        unquote(stream[TestEntity].allowFiltering)
      }
      q.ast mustEqual n.ast
    }
    "multiple params" in {
      val q: Quoted[(Int, Int, Int) => EntityStream[TestEntity]] = quote {
        (i: Int, j: Int, k: Int) =>
          stream[TestEntity].allowFiltering
      }
      val n = quote { (i: Int, j: Int, k: Int) =>
        unquote(stream[TestEntity].allowFiltering)
      }
      q.ast mustEqual n.ast
    }
  }
}
