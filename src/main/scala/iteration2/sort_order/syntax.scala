package iteration2.sort_order

import common.OrderingUtil
import iteration2.sort_order.SortOrder._

object syntax {

  private object OptionOrdering {

    def apply[A](rootOrdering: Ordering[A],
                 emptyFirst: Boolean): Ordering[Option[A]] =
      if (emptyFirst)
        OptionOrdering.emptyFirst(rootOrdering)
      else
        OptionOrdering.emptyLast(rootOrdering)

    def emptyFirst[A](rootOrdering: Ordering[A]): Ordering[Option[A]] =
      (x: Option[A], y: Option[A]) => (x, y) match {
        case (None, None) => 0
        case (None, _) => -1
        case (_, None) => 1
        case (Some(a), Some(b)) => rootOrdering.compare(a, b)
      }

    def emptyLast[A](rootOrdering: Ordering[A]): Ordering[Option[A]] =
      (x: Option[A], y: Option[A]) => (x, y) match {
        case (None, None) => 0
        case (None, _) => 1
        case (_, None) => -1
        case (Some(a), Some(b)) => rootOrdering.compare(a, b)
      }
  }

  implicit class OrderSyntax(val order: SortOrder) extends AnyVal {

    def optional[A](ordering: Ordering[A]): Ordering[Option[A]] =
      order match {
        case Keep => OrderingUtil.identity
        case Asc(emptyFirst) => OptionOrdering(ordering, emptyFirst)
        case Desc(emptyFirst) => OptionOrdering(ordering.reverse, emptyFirst)
      }

    def apply[A](ordering: Ordering[A]): Ordering[A] =
      order match {
        case Keep => OrderingUtil.identity
        case Asc(_) => ordering
        case Desc(_) => ordering.reverse
      }
  }
}
