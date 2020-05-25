package io.spill.context

import io.spill.Spec
import io.spill.testContext
import io.spill.testContext._
import mirror.Row
import io.spill.MirrorContext
import io.spill.NamingStrategy
import io.spill.idiom.Idiom
import io.spill.MirrorIdiom
import io.spill.TestEntities
import io.spill.Literal
import io.spill.Escape
import io.spill.UpperCase
import io.spill.SnakeCase

class ContextMacroSpec extends Spec {

  "runs actions" - {
    "non-parametrized" - {
      "normal" in {
        val q = quote {
          qr1.delete
        }
        testContext.run(q).string mustEqual
          """querySchema("TestEntity").delete"""
      }
      "infix" in {
        val q = quote {
          infix"STRING".as[Action[TestEntity]]
        }
        testContext.run(q).string mustEqual
          """infix"STRING""""
      }
      "dynamic" in {
        val q = quote {
          qr1.delete
        }
        testContext.run(q.dynamic).string mustEqual
          """querySchema("TestEntity").delete"""
      }
      "dynamic type param" in {
        def test[T: SchemaMeta] = quote(stream[T].delete)
        val r = testContext.run(test[TestEntity])
        r.string mustEqual """querySchema("TestEntity").delete"""
      }
    }
    "parametrized" - {
      "normal" in {
        val q = quote {
          qr1.filter(t => t.s == lift("a")).delete
        }
        val r = testContext.run(q)
        r.string mustEqual """querySchema("TestEntity").filter(t => t.s == ?).delete"""
        r.prepareRow mustEqual Row("a")
      }
      "infix" in {
        val q = quote {
          infix"t = ${lift("a")}".as[Action[TestEntity]]
        }
        val r = testContext.run(q)
        r.string mustEqual s"""infix"t = $${?}""""
        r.prepareRow mustEqual Row("a")
      }
      "dynamic" in {
        val q = quote {
          infix"t = ${lift("a")}".as[Action[TestEntity]]
        }
        val r = testContext.run(q.dynamic)
        r.string mustEqual s"""infix"t = $${?}""""
        r.prepareRow mustEqual Row("a")
      }
      "dynamic type param" in {
        import language.reflectiveCalls
        def test[T <: { def i: Int }: SchemaMeta] = quote {
          stream[T].filter(t => t.i == lift(1)).delete
        }
        val r = testContext.run(test[TestEntity])
        r.string mustEqual """querySchema("TestEntity").filter(t => t.i == ?).delete"""
        r.prepareRow mustEqual Row(1)
      }
    }
  }

  "translate actions" - {
    "non-parametrized" - {
      "normal" in {
        val q = quote {
          qr1.delete
        }
        testContext.translate(q) mustEqual
          """querySchema("TestEntity").delete"""
      }
      "infix" in {
        val q = quote {
          infix"STRING".as[Action[TestEntity]]
        }
        testContext.translate(q) mustEqual
          """infix"STRING""""
      }
      "dynamic" in {
        val q = quote {
          qr1.delete
        }
        testContext.translate(q.dynamic) mustEqual
          """querySchema("TestEntity").delete"""
      }
      "dynamic type param" in {
        def test[T: SchemaMeta] = quote(stream[T].delete)
        testContext.translate(test[TestEntity]) mustEqual
          """querySchema("TestEntity").delete"""
      }
    }
    "parametrized" - {
      "normal" in {
        val q = quote {
          qr1.filter(t => t.s == lift("a")).delete
        }
        testContext.translate(q) mustEqual
          """querySchema("TestEntity").filter(t => t.s == 'a').delete"""
      }
      "infix" in {
        val q = quote {
          infix"t = ${lift("a")}".as[Action[TestEntity]]
        }
        testContext.translate(q) mustEqual s"""infix"t = $${'a'}""""
      }
      "dynamic" in {
        val q = quote {
          infix"t = ${lift("a")}".as[Action[TestEntity]]
        }
        testContext.translate(q.dynamic) mustEqual s"""infix"t = $${'a'}""""
      }
      "dynamic type param" in {
        import language.reflectiveCalls
        def test[T <: { def i: Int }: SchemaMeta] = quote {
          stream[T].filter(t => t.i == lift(1)).delete
        }
        testContext.translate(test[TestEntity]) mustEqual
          """querySchema("TestEntity").filter(t => t.i == 1).delete"""
      }
    }
  }

  "runs queries" - {
    "non-parametrized" - {
      "normal" in {
        val q = quote {
          qr1.map(t => t.s)
        }
        testContext.run(q).string mustEqual
          """querySchema("TestEntity").map(t => t.s)"""
      }
      "infix" in {
        val q = quote {
          infix"STRING".as[Stream[TestEntity]].map(t => t.s)
        }
        testContext.run(q).string mustEqual
          """infix"STRING".map(t => t.s)"""
      }
      "dynamic" in {
        val q = quote {
          qr1.map(t => t.s)
        }
        testContext.run(q.dynamic).string mustEqual
          """querySchema("TestEntity").map(t => t.s)"""
      }
      "dynamic type param" in {
        def test[T: SchemaMeta] = quote(stream[T])
        val r = testContext.run(test[TestEntity])
        r.string mustEqual """querySchema("TestEntity").map(x => (x.s, x.i, x.l, x.o))"""
      }
    }
    "parametrized" - {
      "normal" in {
        val q = quote {
          qr1.filter(t => t.s == lift("a"))
        }
        val r = testContext.run(q)
        r.string mustEqual """querySchema("TestEntity").filter(t => t.s == ?).map(t => (t.s, t.i, t.l, t.o))"""
        r.prepareRow mustEqual Row("a")
      }

      "value class" in {
        case class Entity(x: ValueClass)
        val q = quote {
          stream[Entity].filter(t => t.x == lift(ValueClass(1)))
        }
        val r = testContext.run(q)
        r.string mustEqual """querySchema("Entity").filter(t => t.x == ?).map(t => t.x)"""
        r.prepareRow mustEqual Row(1)
      }
      "generic value class" in {
        case class Entity(x: GenericValueClass[Int])
        val q = quote {
          stream[Entity].filter(t => t.x == lift(GenericValueClass(1)))
        }
        val r = testContext.run(q)
        r.string mustEqual """querySchema("Entity").filter(t => t.x == ?).map(t => t.x)"""
        r.prepareRow mustEqual Row(1)
      }
      "infix" in {
        val q = quote {
          infix"SELECT ${lift("a")}".as[Stream[String]]
        }
        val r = testContext.run(q)
        r.string mustEqual s"""infix"SELECT $${?}".map(x => x)"""
        r.prepareRow mustEqual Row("a")
      }
      "dynamic" in {
        val q = quote {
          qr1.filter(t => t.s == lift("a"))
        }
        val r = testContext.run(q.dynamic)
        r.string mustEqual """querySchema("TestEntity").filter(t => t.s == ?).map(t => (t.s, t.i, t.l, t.o))"""
        r.prepareRow mustEqual Row("a")
      }
      "dynamic type param" in {
        def test[T: SchemaMeta: QueryMeta] = quote {
          stream[T].map(t => lift(1))
        }
        val r = testContext.run(test[TestEntity])
        r.string mustEqual """querySchema("TestEntity").map(t => ?)"""
        r.prepareRow mustEqual Row(1)
      }
    }
    "aggregated" in {
      val q = quote {
        qr1.map(t => t.i).max
      }
      testContext.run(q).string mustEqual
        """querySchema("TestEntity").map(t => t.i).max"""
    }
  }

  "translate queries" - {
    "non-parametrized" - {
      "normal" in {
        val q = quote {
          qr1.map(t => t.s)
        }
        testContext.translate(q) mustEqual
          """querySchema("TestEntity").map(t => t.s)"""
      }
      "infix" in {
        val q = quote {
          infix"STRING".as[Stream[TestEntity]].map(t => t.s)
        }
        testContext.translate(q) mustEqual
          """infix"STRING".map(t => t.s)"""
      }
      "dynamic" in {
        val q = quote {
          qr1.map(t => t.s)
        }
        testContext.translate(q.dynamic) mustEqual
          """querySchema("TestEntity").map(t => t.s)"""
      }
      "dynamic type param" in {
        def test[T: SchemaMeta] = quote(stream[T])
        testContext.translate(test[TestEntity]) mustEqual
          """querySchema("TestEntity").map(x => (x.s, x.i, x.l, x.o))"""
      }
    }
    "parametrized" - {
      "normal" in {
        val q = quote {
          qr1.filter(t => t.s == lift("a"))
        }
        testContext.translate(q) mustEqual
          """querySchema("TestEntity").filter(t => t.s == 'a').map(t => (t.s, t.i, t.l, t.o))"""
      }

      "value class" in {
        case class Entity(x: ValueClass)
        val q = quote {
          stream[Entity].filter(t => t.x == lift(ValueClass(1)))
        }
        testContext.translate(q) mustEqual
          """querySchema("Entity").filter(t => t.x == 1).map(t => t.x)"""
      }
      "generic value class" in {
        case class Entity(x: GenericValueClass[Int])
        val q = quote {
          stream[Entity].filter(t => t.x == lift(GenericValueClass(1)))
        }
        testContext.translate(q) mustEqual
          """querySchema("Entity").filter(t => t.x == 1).map(t => t.x)"""
      }
      "infix" in {
        val q = quote {
          infix"SELECT ${lift("a")}".as[Stream[String]]
        }
        testContext.translate(q) mustEqual s"""infix"SELECT $${'a'}".map(x => x)"""
      }
      "dynamic" in {
        val q = quote {
          qr1.filter(t => t.s == lift("a"))
        }
        testContext.translate(q.dynamic) mustEqual
          """querySchema("TestEntity").filter(t => t.s == 'a').map(t => (t.s, t.i, t.l, t.o))"""
      }
      "dynamic type param" in {
        def test[T: SchemaMeta: QueryMeta] = quote {
          stream[T].map(t => lift(1))
        }
        testContext.translate(test[TestEntity]) mustEqual
          """querySchema("TestEntity").map(t => 1)"""
      }
    }
    "aggregated" in {
      val q = quote {
        qr1.map(t => t.i).max
      }
      testContext.translate(q) mustEqual
        """querySchema("TestEntity").map(t => t.i).max"""
    }
  }

  "fails if there's a free variable" in {
    val q = {
      val i = 1
      quote {
        qr1.filter(_.i == i)
      }
    }
    "testContext.run(q)" mustNot compile
    "testContext.translate(q)" mustNot compile
  }

  "falls back to dynamic queries if idiom/naming are not known" in {
    import language.existentials
    def test(ctx: MirrorContext[_ <: Idiom, _ <: NamingStrategy]) = {
      import ctx._
      ctx.run(stream[TestEntity])
    }

    def translateTest(ctx: MirrorContext[_ <: Idiom, _ <: NamingStrategy]) = {
      import ctx._
      ctx.translate(stream[TestEntity])
    }

    test(testContext).string mustEqual """querySchema("TestEntity").map(x => (x.s, x.i, x.l, x.o))"""
    translateTest(testContext) mustEqual """querySchema("TestEntity").map(x => (x.s, x.i, x.l, x.o))"""
  }

  "supports composite naming strategies" - {
    "two" in {
      object ctx
        extends MirrorContext(MirrorIdiom, NamingStrategy(Literal, Escape))
        with TestEntities
      import ctx._
      ctx
        .run(stream[TestEntity])
        .string mustEqual """querySchema("TestEntity").map(x => (x.s, x.i, x.l, x.o))"""
      ctx.translate(stream[TestEntity]) mustEqual """querySchema("TestEntity").map(x => (x.s, x.i, x.l, x.o))"""
    }
    "three" in {
      object ctx
        extends MirrorContext(
          MirrorIdiom,
          NamingStrategy(Literal, Escape, UpperCase)
        )
        with TestEntities
      import ctx._
      ctx
        .run(stream[TestEntity])
        .string mustEqual """querySchema("TestEntity").map(x => (x.s, x.i, x.l, x.o))"""
      ctx.translate(stream[TestEntity]) mustEqual """querySchema("TestEntity").map(x => (x.s, x.i, x.l, x.o))"""
    }
    "four" in {
      object ctx
        extends MirrorContext(
          MirrorIdiom,
          NamingStrategy(Literal, Escape, UpperCase, SnakeCase)
        )
        with TestEntities
      import ctx._
      ctx
        .run(stream[TestEntity])
        .string mustEqual """querySchema("TestEntity").map(x => (x.s, x.i, x.l, x.o))"""
      ctx.translate(stream[TestEntity]) mustEqual """querySchema("TestEntity").map(x => (x.s, x.i, x.l, x.o))"""
    }
  }
}
