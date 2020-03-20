package iteration2.sort_order

import common.OrderingUtil
import iteration2.sort_order.SortOrder._

object syntax {

  private object OptionOrdering {

    def apply[A](rootOrdering: Ordering[A],
                 nullsFirst: Boolean): Ordering[Option[A]] =
      if (nullsFirst)
        OptionOrdering.nullsFirst(rootOrdering)
      else
        OptionOrdering.nullsLast(rootOrdering)

    def nullsFirst[A](rootOrdering: Ordering[A]): Ordering[Option[A]] =
      (x: Option[A], y: Option[A]) => (x, y) match {
        case (None, None) => 0
        case (None, _) => -1
        case (_, None) => 1
        case (Some(a), Some(b)) => rootOrdering.compare(a, b)
      }

    def nullsLast[A](rootOrdering: Ordering[A]): Ordering[Option[A]] =
      (x: Option[A], y: Option[A]) => (x, y) match {
        case (None, None) => 0
        case (None, _) => 1
        case (_, None) => -1
        case (Some(a), Some(b)) => rootOrdering.compare(a, b)
      }
  }

  // Note this decision is intentional since if we apply order to ordering, we may forget it and there won't
  // be a compile error
  implicit class OrderSyntax(val order: SortOrder) extends AnyVal {

    def optional[A](ordering: Ordering[A]): Ordering[Option[A]] =
      order match {
        case Any => OrderingUtil.identity
        case Asc(nullsFirst) => OptionOrdering(ordering, nullsFirst)
        case Desc(nullsFirst) => OptionOrdering(ordering.reverse, nullsFirst)
      }

    def apply[A](ordering: Ordering[A]): Ordering[A] =
      order match {
        case Any => OrderingUtil.identity
        case Asc(_) => ordering
        case Desc(_) => ordering.reverse
      }
  }
}
