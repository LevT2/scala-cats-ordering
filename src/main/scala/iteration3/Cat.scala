package iteration3

import java.time.LocalDate

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
