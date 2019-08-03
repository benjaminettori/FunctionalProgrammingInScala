package simplified
import com.softwaremill.quicklens._

object Lenses {
  // Lensing is a way to copy deeply nested objects in an immutable way
  // we use QuickLens, the simplest lens library

  // much more complex is the Monocle library, which provide a lens path to specific nested structures within a type

  case class User (
                  id: Int,
                  name: Name,
                  billingInfo: BillingInfo,
                  phone: String,
                  email: String
                  )

  case class Name (firstName: String, lastName: String)

  case class Address(
                    street1: String,
                    street2: String,
                    city: String,
                    state: String,
                    zip: String
                    )

  case class CreditCard (name: Name, number: String, month: Int, year: Int, cvv: String)

  case class BillingInfo(creditCards: Seq[CreditCard])

  def main(args: Array[String]) = {


    val user = User(
      id = 1,
      name = Name(firstName = "Ben", lastName = "Ettori"),
      billingInfo = BillingInfo(
        creditCards = Seq(
          CreditCard(
            name = Name("Ben", "Ettori"),
            number = "11223344",
            month = 3,
            year = 2020,
            cvv = ""
          )
        )
      ),
      phone = "215-422-2121",
      email= "b@b.com"
    )

    val user1 = user.modify(_.phone).setTo("567-980-2392").modify(_.name.firstName).setTo("John")
    println(user1)
  }

}
