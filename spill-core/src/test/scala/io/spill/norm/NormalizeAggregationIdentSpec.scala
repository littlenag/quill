package io.spill.norm

import io.spill.Spec
import io.spill.testContext._

class NormalizeAggregationIdentSpec extends Spec {
  "multiple select" in {
    val q = quote {
      qr1.groupBy(p => p.i).map {
        case (i, qrs) => i -> qrs.map(_.l).sum
      }
    }
    val n = quote {
      qr1.groupBy(p => p.i).map { p =>
        p._1 -> p._2.map(p => p.l).sum
      }
    }
    Normalize(q.ast) mustEqual (n.ast)
  }
}
