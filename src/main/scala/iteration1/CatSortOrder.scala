package iteration1

import Ordering.{Boolean => BooleanO, Int => IntO, String => StringO}
import SortOrder.OrderSyntax

case class CatSortOrder(idOrder: SortOrder,
                        nameOrder: SortOrder,
                        availableOrder: SortOrder) {

  def toOrdering: Ordering[Cat] = {
    Ordering
      .Tuple3(idOrder(IntO), nameOrder(StringO), availableOrder(BooleanO))
      .on[Cat](cat => (cat.age, cat.name, cat.available))
  }
}
