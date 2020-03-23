package iteration2

import iteration2.sort_order.SortOrder
import iteration2.sort_order.syntax._

import Ordering.{Boolean => BooleanO, Int => IntO, String => StringO}

object CatOrdering {

  def toOrdering(idOrder: SortOrder,
                 nameOrder: SortOrder,
                 availableOrder: SortOrder,
                 ownerOrder: SortOrder): Ordering[Cat] = {
    Ordering
      .Tuple4(idOrder(IntO), nameOrder(StringO), availableOrder(BooleanO), ownerOrder.optional(StringO))
      .on[Cat](cat => (cat.age, cat.name, cat.available, cat.owner))
  }
}
