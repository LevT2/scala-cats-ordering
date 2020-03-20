package iteration2

import iteration2.sort_order.SortOrder
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class OrderingTest2 extends FlatSpec with Matchers with ScalaCheckDrivenPropertyChecks  {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2000, minSize = 100, sizeRange = 50)

  implicit val catGen: Gen[Cat] = for {
    age <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
    name <- Gen.alphaStr
    available <- Gen.oneOf(true, false)
    owner <- Gen.option(Gen.alphaStr)
  } yield Cat(age, name, available, owner)

  val naturalOrdering: Ordering[Cat] = Ordering.fromLessThan((c1, c2) =>
    (c1.age < c2.age) ||
    (c1.age == c2.age) && (c1.name < c2.name) ||
    (c1.age == c2.age) && (c1.name == c2.name) && (c1.available < c2.available) ||
    (c1.age == c2.age) && (c1.name == c2.name) && (c1.available == c2.available) && Ordering[Option[String]].lt(c1.owner, c2.owner)
  )

  "Ascending sort order (nulls first) on all fields" should "be identical to natural ordering" in {
    val providedOrdering: Ordering[Cat] = CatSortOrder(SortOrder.Asc.nullsFirst, SortOrder.Asc.nullsFirst, SortOrder.Asc.nullsFirst, SortOrder.Asc.nullsFirst).toOrdering

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(naturalOrdering))
    }
  }

  "Descending sort order (nulls last) on all fields" should "be identical to reversed natural ordering" in {
    val providedOrdering: Ordering[Cat] = CatSortOrder(SortOrder.Desc.nullsLast, SortOrder.Desc.nullsLast, SortOrder.Desc.nullsLast, SortOrder.Desc.nullsLast).toOrdering

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(naturalOrdering.reverse))
    }
  }

  "Any sort order on all fields" should "not change the order of elements" in {
    val providedOrdering: Ordering[Cat] = CatSortOrder(SortOrder.Any, SortOrder.Any, SortOrder.Any, SortOrder.Any).toOrdering

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats)
    }
  }

  "Ascending sort order only by the \"name\" field" should "be the same as natural ordering by that field" in {
    val providedOrdering: Ordering[Cat] = CatSortOrder(SortOrder.Any, SortOrder.Asc.nullsFirst, SortOrder.Any, SortOrder.Any).toOrdering

    val fieldOrdering: Ordering[Cat] = Ordering.by(_.name)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Descending sort order only by the \"name\" field" should "be the same as reversed natural ordering by that field" in {
    val providedOrdering: Ordering[Cat] = CatSortOrder(SortOrder.Any, SortOrder.Desc.nullsFirst, SortOrder.Any, SortOrder.Any).toOrdering

    val fieldOrdering: Ordering[Cat] = Ordering.by[Cat, String](_.name).reverse

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Ascending sort order (nulls first) only by the \"owner\" field" should "be the same as natural ordering by that field" in {
    val providedOrdering: Ordering[Cat] = CatSortOrder(SortOrder.Any, SortOrder.Any, SortOrder.Any, SortOrder.Asc.nullsFirst).toOrdering

    val fieldOrdering: Ordering[Cat] = Ordering.by(_.owner)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Descending sort order (nulls last) only by the \"owner\" field" should "be the same as reversed natural ordering by that field" in {
    val providedOrdering: Ordering[Cat] = CatSortOrder(SortOrder.Any, SortOrder.Any, SortOrder.Any, SortOrder.Desc.nullsLast).toOrdering

    val fieldOrdering: Ordering[Cat] = Ordering.by[Cat, Option[String]](_.owner).reverse

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }
}
