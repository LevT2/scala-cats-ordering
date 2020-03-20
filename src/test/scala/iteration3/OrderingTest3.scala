package iteration3

import java.time.{LocalDate, ZoneOffset}

import iteration3.sort_order.SortOrder
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class OrderingTest3 extends FlatSpec with Matchers with ScalaCheckDrivenPropertyChecks  {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2000, minSize = 100, sizeRange = 50)

  implicit val catGen: Gen[Cat] = for {
    age <- Gen.chooseNum(Int.MinValue, Int.MaxValue)
    name <- Gen.alphaStr
    available <- Gen.oneOf(true, false)
    owner <- Gen.option(Gen.alphaStr)
    breed <- Gen.alphaStr
    furColor <- Gen.alphaStr
    eyeColor <- Gen.alphaStr
    registrationId <- Gen.alphaNumStr
    lastHealthCheck <- Gen.option(Gen.calendar.map(_.toInstant.atZone(ZoneOffset.UTC).toLocalDate))
    urgentSell <- Gen.oneOf(true, false)
  } yield Cat(age, name, available, owner, breed, furColor, eyeColor, registrationId, lastHealthCheck, urgentSell)

  val naturalOrdering: Ordering[Cat] = Ordering.comparatorToOrdering(
    Ordering.by[Cat, Int](_.age)
      .thenComparing(_.name) // d
      .thenComparing(c => Boolean.box(c.available)) // Boolean in Scala doesn't implement comparable, literals `1` are inferred to be Int, not Integer
      .thenComparing(Ordering.by[Cat, Option[String]](_.owner)(Ordering[Option[String]]))
      .thenComparing(_.breed)
      .thenComparing(_.furColor)
      .thenComparing(_.eyeColor)
      .thenComparing(_.registrationId)
      .thenComparing(Ordering.by[Cat, Option[LocalDate]](_.lastHealthCheck)(Ordering.Option(Ordering.by[LocalDate, Long](_.toEpochDay))))
      .thenComparing(c => Boolean.box(c.urgentSell))
  )

  "Ascending sort order (nulls first) on all fields" should "be identical to natural ordering" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map(
      CatSortField.Age ->             SortOrder.Asc.nullsFirst,
      CatSortField.Name ->            SortOrder.Asc.nullsFirst,
      CatSortField.Available ->       SortOrder.Asc.nullsFirst,
      CatSortField.Owner ->           SortOrder.Asc.nullsFirst,
      CatSortField.Breed ->           SortOrder.Asc.nullsFirst,
      CatSortField.FurColor ->        SortOrder.Asc.nullsFirst,
      CatSortField.EyeColor ->        SortOrder.Asc.nullsFirst,
      CatSortField.RegistrationId ->  SortOrder.Asc.nullsFirst,
      CatSortField.LastHealthCheck -> SortOrder.Asc.nullsFirst,
      CatSortField.UrgentSell ->      SortOrder.Asc.nullsFirst
    )
    val providedOrdering: Ordering[Cat] = CatSortField.toOrdering(fieldMap)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(naturalOrdering))
    }
  }

  "Descending sort order (nulls last) on all fields" should "be identical to reversed natural ordering" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map(
      CatSortField.Age ->             SortOrder.Desc.nullsLast,
      CatSortField.Name ->            SortOrder.Desc.nullsLast,
      CatSortField.Available ->       SortOrder.Desc.nullsLast,
      CatSortField.Owner ->           SortOrder.Desc.nullsLast,
      CatSortField.Breed ->           SortOrder.Desc.nullsLast,
      CatSortField.FurColor ->        SortOrder.Desc.nullsLast,
      CatSortField.EyeColor ->        SortOrder.Desc.nullsLast,
      CatSortField.RegistrationId ->  SortOrder.Desc.nullsLast,
      CatSortField.LastHealthCheck -> SortOrder.Desc.nullsLast,
      CatSortField.UrgentSell ->      SortOrder.Desc.nullsLast
    )
    val providedOrdering: Ordering[Cat] = CatSortField.toOrdering(fieldMap)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(naturalOrdering.reverse))
    }
  }

  "No sort order on all fields" should "not change the order of elements" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map.empty
    val providedOrdering: Ordering[Cat] = CatSortField.toOrdering(fieldMap)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats)
    }
  }

  "Ascending sort order only by the \"name\" field" should "be the same as natural ordering by that field" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map(
      CatSortField.Name -> SortOrder.Asc.nullsFirst,
    )
    val providedOrdering: Ordering[Cat] = CatSortField.toOrdering(fieldMap)

    val fieldOrdering: Ordering[Cat] = Ordering.by(_.name)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Descending sort order only by the \"name\" field" should "be the same as reversed natural ordering by that field" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map(
      CatSortField.Name -> SortOrder.Desc.nullsFirst,
    )
    val providedOrdering: Ordering[Cat] = CatSortField.toOrdering(fieldMap)

    val fieldOrdering: Ordering[Cat] = Ordering.by[Cat, String](_.name).reverse

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Ascending sort order (nulls first) only by the \"owner\" field" should "be the same as natural ordering by that field" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map(
      CatSortField.Owner -> SortOrder.Asc.nullsFirst,
    )
    val providedOrdering: Ordering[Cat] = CatSortField.toOrdering(fieldMap)

    val fieldOrdering: Ordering[Cat] = Ordering.by(_.owner)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Descending sort order (nulls last) only by the \"owner\" field" should "be the same as reversed natural ordering by that field" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map(
      CatSortField.Owner -> SortOrder.Desc.nullsLast,
    )
    val providedOrdering: Ordering[Cat] = CatSortField.toOrdering(fieldMap)

    val fieldOrdering: Ordering[Cat] = Ordering.by[Cat, Option[String]](_.owner).reverse

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }
}
