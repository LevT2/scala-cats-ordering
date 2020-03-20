package iteration3

import java.time.LocalDate

object Cat {

  val values: List[Cat] =
    List(
      Cat(8, "Hana", available = true, Some("Hattori Hanzo"), "Persian", "White", "Brown", "20200102", None, urgentSell = true),
      Cat(4, "Momo", available = false, None, "Bengal", "Leopard", "Green", "20191203", Some(LocalDate.of(2018, 1, 1)), urgentSell = false),
      Cat(2, "Shiro", available = true, None, "Sphynx", "None", "Blue", "20191102", None, urgentSell = false),
      Cat(2, "Fuku", available = true, Some("Hattori Hanzo"), "Munchkin", "Black", "Black", "20180325", Some(LocalDate.of(2019, 8, 20)), urgentSell = false),
      Cat(9, "Kuro", available = false, Some("Shinzo Abe"), "Siberian", "White", "Blue", "20150508", None, urgentSell = true),
      Cat(7, "Kuro", available = false, None, "Chartreux", "Grey", "Yellow", "20160309", Some(LocalDate.of(2019, 2, 5)), urgentSell = false)
    )
}

case class Cat(age: Int,
               name: String,
               available: Boolean,
               owner: Option[String],
               breed: String,
               furColor: String,
               eyeColor: String,
               registrationId: String,
               lastHealthCheck: Option[LocalDate],
               urgentSell: Boolean)
