package iteration1

import iteration1.syntax._

object CatOrdering {

  def of(idOrder: SortOrder,
         nameOrder: SortOrder,
         availableOrder: SortOrder): Ordering[Cat] =
    Ordering
      .Tuple3(idOrder(Ordering.Int), nameOrder(Ordering.String), availableOrder(Ordering.Boolean))
      .on[Cat](cat => (cat.age, cat.name, cat.available))
}
