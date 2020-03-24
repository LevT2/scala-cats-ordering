package iteration3

import java.time.{LocalDate, ZoneOffset}

import iteration3.sort_order.SortOrder
import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class OrderingTest3 extends FlatSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  override implicit val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 2000, minSize = 50, sizeRange = 20)

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

  val naturalOrdering: Ordering[Cat] =
    Ordering.by[Cat, Int](_.age)
      .orElseBy(_.name)
      .orElseBy(_.available)
      .orElseBy(_.owner)
      .orElseBy(_.breed)
      .orElseBy(_.furColor)
      .orElseBy(_.eyeColor)
      .orElseBy(_.registrationId)
      .orElseBy(_.lastHealthCheck)(Ordering.Option(Ordering.by[LocalDate, Long](_.toEpochDay)))
      .orElseBy(_.urgentSell)

  "Ascending sort order (empty first) on all fields" should "be identical to natural ordering" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map(
      CatSortField.Age ->             SortOrder.Asc.emptyFirst,
      CatSortField.Name ->            SortOrder.Asc.emptyFirst,
      CatSortField.Available ->       SortOrder.Asc.emptyFirst,
      CatSortField.Owner ->           SortOrder.Asc.emptyFirst,
      CatSortField.Breed ->           SortOrder.Asc.emptyFirst,
      CatSortField.FurColor ->        SortOrder.Asc.emptyFirst,
      CatSortField.EyeColor ->        SortOrder.Asc.emptyFirst,
      CatSortField.RegistrationId ->  SortOrder.Asc.emptyFirst,
      CatSortField.LastHealthCheck -> SortOrder.Asc.emptyFirst,
      CatSortField.UrgentSell ->      SortOrder.Asc.emptyFirst
    )
    val providedOrdering: Ordering[Cat] = CatSortField.toDefaultOrdering(fieldMap)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(naturalOrdering))
    }
  }

  "Descending sort order (empty last) on all fields" should "be identical to reversed natural ordering" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map(
      CatSortField.Age ->             SortOrder.Desc.emptyLast,
      CatSortField.Name ->            SortOrder.Desc.emptyLast,
      CatSortField.Available ->       SortOrder.Desc.emptyLast,
      CatSortField.Owner ->           SortOrder.Desc.emptyLast,
      CatSortField.Breed ->           SortOrder.Desc.emptyLast,
      CatSortField.FurColor ->        SortOrder.Desc.emptyLast,
      CatSortField.EyeColor ->        SortOrder.Desc.emptyLast,
      CatSortField.RegistrationId ->  SortOrder.Desc.emptyLast,
      CatSortField.LastHealthCheck -> SortOrder.Desc.emptyLast,
      CatSortField.UrgentSell ->      SortOrder.Desc.emptyLast
    )
    val providedOrdering: Ordering[Cat] = CatSortField.toDefaultOrdering(fieldMap)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(naturalOrdering.reverse))
    }
  }

  "No sort order on all fields" should "not change the order of elements" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map.empty
    val providedOrdering: Ordering[Cat] = CatSortField.toDefaultOrdering(fieldMap)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats)
    }
  }

  "Ascending sort order only by the \"name\" field" should "be the same as natural ordering by that field" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map(
      CatSortField.Name -> SortOrder.Asc.emptyFirst,
    )
    val providedOrdering: Ordering[Cat] = CatSortField.toDefaultOrdering(fieldMap)

    val fieldOrdering: Ordering[Cat] = Ordering.by(_.name)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Descending sort order only by the \"name\" field" should "be the same as reversed natural ordering by that field" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map(
      CatSortField.Name -> SortOrder.Desc.emptyFirst,
    )
    val providedOrdering: Ordering[Cat] = CatSortField.toDefaultOrdering(fieldMap)

    val fieldOrdering: Ordering[Cat] = Ordering.by[Cat, String](_.name).reverse

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Ascending sort order (empty first) only by the \"owner\" field" should "be the same as natural ordering by that field" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map(
      CatSortField.Owner -> SortOrder.Asc.emptyFirst,
    )
    val providedOrdering: Ordering[Cat] = CatSortField.toDefaultOrdering(fieldMap)

    val fieldOrdering: Ordering[Cat] = Ordering.by(_.owner)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Descending sort order (empty last) only by the \"owner\" field" should "be the same as reversed natural ordering by that field" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map(
      CatSortField.Owner -> SortOrder.Desc.emptyLast,
    )
    val providedOrdering: Ordering[Cat] = CatSortField.toDefaultOrdering(fieldMap)

    val fieldOrdering: Ordering[Cat] = Ordering.by[Cat, Option[String]](_.owner).reverse

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Ascending sort order (empty last) only by the \"owner\" field" should "be correct" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map(
      CatSortField.Owner -> SortOrder.Asc.emptyLast,
    )
    val providedOrdering: Ordering[Cat] = CatSortField.toDefaultOrdering(fieldMap)
    val fieldOrdering: Ordering[Cat] = Ordering.fromLessThan[Option[String]] {
      case (None, None) => false
      case (None, _) => false
      case (_, None) => true
      case (Some(x), Some(y)) => x < y
    }.on(_.owner)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Descending sort order (empty first) only by the \"owner\" field" should "be correct" in {
    val fieldMap: Map[CatSortField, SortOrder] = Map(
      CatSortField.Owner -> SortOrder.Desc.emptyFirst,
    )
    val providedOrdering: Ordering[Cat] = CatSortField.toDefaultOrdering(fieldMap)

    val fieldOrdering: Ordering[Cat] = Ordering.fromLessThan[Option[String]] {
      case (None, None) => false
      case (None, _) => true
      case (_, None) => false
      case (Some(x), Some(y)) => x > y
    }.on(_.owner)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }

  "Customized sort order" should "be correct" in {
    val fieldMap: Seq[(CatSortField, SortOrder)] = Seq(
      CatSortField.Owner -> SortOrder.Desc.emptyLast,
      CatSortField.Name -> SortOrder.Asc.emptyFirst,
      CatSortField.Available -> SortOrder.Asc.emptyFirst
    )
    val providedOrdering: Ordering[Cat] = CatSortField.toCustomizedOrdering(fieldMap)

    val fieldOrdering: Ordering[Cat] = Ordering.by[Cat, Option[String]](_.owner).reverse
      .orElseBy(_.name)
      .orElseBy(_.available)

    forAll(Gen.listOf(catGen)) { cats =>
      cats.sorted(providedOrdering) should equal (cats.sorted(fieldOrdering))
    }
  }
}
