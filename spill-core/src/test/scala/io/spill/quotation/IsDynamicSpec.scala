package io.spill.quotation

import io.spill.Spec
import io.spill.ast.Dynamic
import io.spill.ast.Property
import io.spill.ast.Renameable.Fixed
import io.spill.ast.Visibility.Visible
import io.spill.testContext.qr1
import io.spill.testContext.qrRegular

class IsDynamicSpec extends Spec {

  "detects if the quotation has dynamic parts" - {
    "true" - {
      "fully dynamic" in {
        IsDynamic(Dynamic(1)) mustEqual true
      }
      "partially dynamic" in {
        IsDynamic(Property(Dynamic(1), "a")) mustEqual true
      }
      "partially dynamic - fixed" in {
        IsDynamic(Property.Opinionated(Dynamic(1), "a", Fixed, Visible)) mustEqual true
      }
    }
    "false" in {
      IsDynamic(qr1.ast) mustEqual false
    }
    "false when using CaseClass" in {
      IsDynamic(qrRegular.ast) mustEqual false
    }
  }
}
