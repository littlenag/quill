package io.spill.norm.capture

import io.spill.Spec
import io.spill.testContext._

class AvoidAliasConflictComplexSpec extends Spec {

  "properly aliases explicit join sets" - {
    import io.spill.norm.Normalize
    import scala.language.reflectiveCalls

    case class Person(id: Int, name: String)
    case class Address(id: Int, ownerFk: Int, street: String)
    case class Room(addressId: Int, stuff: String)

    "in tail clause" in {
      def fun[T <: { def id: Int }] = quote { (tbl: Query[T]) =>
        for {
          t <- tbl
          a <- query[Address].join(a => a.ownerFk == t.id)
        } yield (t, a)
      }

      def funExpect[T <: { def id: Int }] = quote { (tbl: Query[T]) =>
        for {
          t <- tbl
          a <- query[Address].join(a1 => a1.ownerFk == t.id)
        } yield (t, a)
      }

      val q = quote {
        fun[Person](query[Person].filter(a => a.name == "Joe"))
      }
      val expect = quote {
        funExpect[Person](query[Person].filter(a => a.name == "Joe"))
      }
      Normalize(q.ast) mustEqual Normalize(expect.ast)
    }

    "in middle clause" in {
      def fun[T <: { def id: Int }] = quote { (tbl: Query[T]) =>
        for {
          t <- tbl
          a <- query[Address].join(a => a.ownerFk == t.id)
          r <- query[Room].join(r => r.addressId == a.id)
        } yield (t, a, r)
      }

      def funExpect[T <: { def id: Int }] = quote { (tbl: Query[T]) =>
        for {
          t <- tbl
          a <- query[Address].join(a1 => a1.ownerFk == t.id)
          r <- query[Room].join(r => r.addressId == a.id)
        } yield (t, a, r)
      }

      val q = quote {
        fun[Person](query[Person].filter(a => a.name == "Joe"))
      }
      val expect = quote {
        funExpect[Person](query[Person].filter(a => a.name == "Joe"))
      }
      Normalize(q.ast) mustEqual Normalize(expect.ast)
    }

    "in middle and end clause" in {
      def fun[T <: { def id: Int }] = quote { (tbl: Query[T]) =>
        for {
          t <- tbl
          a <- query[Address].join(a => a.ownerFk == t.id)
          r <- query[Room].join(a => a.addressId == 1)
        } yield (t, a, r)
      }

      def funExpect[T <: { def id: Int }] = quote { (tbl: Query[T]) =>
        for {
          t <- tbl
          a <- query[Address].join(a1 => a1.ownerFk == t.id)
          r <- query[Room].join(a2 => a2.addressId == 1)
        } yield (t, a, r)
      }

      val q = quote {
        fun[Person](query[Person].filter(a => a.name == "Joe"))
      }
      val expect = quote {
        funExpect[Person](query[Person].filter(a => a.name == "Joe"))
      }
      Normalize(q.ast) mustEqual Normalize(expect.ast)
    }
  }
}
