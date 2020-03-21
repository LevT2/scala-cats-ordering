package iteration1

import common.OrderingUtil
import iteration1.SortOrder.{Any, Asc, Desc}

object syntax {

  // Note this decision is intentional since if we apply order to ordering, we may forget it and there won't
  // be a compile error
  implicit class OrderSyntax(val order: SortOrder) extends AnyVal {

    def apply[A](ordering: Ordering[A]): Ordering[A] =
      order match {
        case Any => OrderingUtil.identity
        case Asc => ordering
        case Desc => ordering.reverse
      }
  }
}
