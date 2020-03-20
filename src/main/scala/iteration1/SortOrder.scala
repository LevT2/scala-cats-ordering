package iteration1

import common.OrderingUtil

sealed trait SortOrder

object SortOrder {

  case object Any extends SortOrder
  case object Asc extends SortOrder
  case object Desc extends SortOrder

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


