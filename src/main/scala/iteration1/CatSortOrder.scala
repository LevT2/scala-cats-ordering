package iteration1

import Ordering.{Boolean => BooleanO, Int => IntO, String => StringO}
import syntax._

case class CatSortOrder(idOrder: SortOrder,
                        nameOrder: SortOrder,
                        availableOrder: SortOrder) {

  def toOrdering: Ordering[Cat] = {
    Ordering
      .Tuple3(idOrder.apply(IntO), nameOrder.apply(StringO), availableOrder.apply(BooleanO))
      .on[Cat](cat => (cat.age, cat.name, cat.available))
  }
}
