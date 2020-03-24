package iteration1

import common.OrderingUtil
import iteration1.SortOrder.{Asc, Desc, Keep}

object syntax {

  implicit class OrderSyntax(val order: SortOrder) extends AnyVal {

    def apply[A](ordering: Ordering[A]): Ordering[A] =
      order match {
        case Keep => OrderingUtil.identity
        case Asc => ordering
        case Desc => ordering.reverse
      }
  }
}
