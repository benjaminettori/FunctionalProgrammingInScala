package simplified

object Modules {

  case class Pizza (crustSize: CrustSize,
                    crustType: CrustType,
                    toppings: List[Topping]
                   )

  case class Address (street1: String,
                      street2: Option[String],
                      city: String,
                      state: String,
                      zipCode: String
                     )

  case class Customer (name:String,
                       phone: String,
                       address: Address
                      )

  case class Order (pizzas: Seq[Pizza])


  sealed trait CrustSize
  case object SmallCrustSize extends CrustSize
  case object MediumCrustSize extends CrustSize
  case object LargeCrustSize extends CrustSize

  sealed trait CrustType
  case object RegularCrustType extends CrustType
  case object ThinCrustType extends CrustType
  case object ThickCrustType extends CrustType

  sealed trait Topping
  case object Cheese extends Topping
  case object Pepperoni extends Topping
  case object Sausage extends Topping
  case object Mushrooms extends Topping

  type Money = BigDecimal

  trait PizzaServiceInterface {
    def addTopping(topping: Topping, pizza: Pizza) : Pizza
    def removeTopping(topping: Topping, pizza: Pizza) : Pizza
    def removeAllToppings(pizza: Pizza) : Pizza

    def updateCrustSize(crustSize: CrustSize, pizza: Pizza) : Pizza
    def updateCrustType(crustType: CrustType, pizza: Pizza) : Pizza

    def calculatePizzaPrice(
                           p: Pizza,
                           toppingsPrices: Map[Topping, Money],
                           crustSizePrices: Map[CrustSize, Money],
                           crustTypePrices: Map[CrustType, Money]
                           ) : Money
  }

  trait PizzaService extends PizzaServiceInterface {

    override def addTopping(topping: Topping, pizza: Pizza): Pizza = pizza.copy(toppings = topping :: pizza.toppings)

    override def removeTopping(topping: Topping, pizza: Pizza): Pizza = pizza.copy(toppings = pizza.toppings.filter(_ != topping))

    override def removeAllToppings(pizza: Pizza): Pizza = pizza.copy(toppings = List[Topping]())

    override def updateCrustSize(crustSize: CrustSize, pizza: Pizza): Pizza = pizza.copy(crustSize = crustSize)

    override def updateCrustType(crustType: CrustType, pizza: Pizza): Pizza = pizza.copy(crustType = crustType)

    override def calculatePizzaPrice(p: Pizza, toppingsPrices: Map[Topping, Money], crustSizePrices: Map[CrustSize, Money], crustTypePrices: Map[CrustType, Money]): Money = {
      val crustPrice = crustSizePrices(p.crustSize) + crustTypePrices(p.crustType)

      val toppingsPrice = p.toppings.foldLeft(BigDecimal(0))((price, topping) => price + toppingsPrices(topping))

      toppingsPrice + crustPrice
    }

  }

  trait PizzaComponentPricesDaoInterface  {
    def getToppingsPrices() : Map[Topping, Money]
    def getCrustSizePrices(): Map[CrustSize, Money]
    def getCrustTypePrices() : Map[CrustType, Money]
  }

  trait PizzaComponentPricesDao extends PizzaComponentPricesDaoInterface {
    override def getToppingsPrices(): Map[Topping, Money] =
      Map(
        Cheese -> BigDecimal(1),
        Pepperoni -> BigDecimal(1),
        Sausage -> BigDecimal(1),
        Mushrooms -> BigDecimal(1)
      )

    override def getCrustSizePrices(): Map[CrustSize, Money] =
      Map(
        SmallCrustSize -> BigDecimal(1),
        MediumCrustSize -> BigDecimal(1),
        LargeCrustSize -> BigDecimal(1)
      )

    override def getCrustTypePrices(): Map[CrustType, Money] =
      Map(
        RegularCrustType -> BigDecimal(1),
        ThinCrustType -> BigDecimal(1),
        ThickCrustType -> BigDecimal(1)
      )
  }

  trait OrderServiceInterface {
    protected def database: PizzaComponentPricesDaoInterface
    def calculateOrderPrice(order: Order) : Money
  }

  trait AbstractOrderService extends OrderServiceInterface {
    this : PizzaService =>
    lazy val toppingsPrices = database.getToppingsPrices()
    lazy val crustTypePrices = database.getCrustTypePrices()
    lazy val crustSizePrices = database.getCrustSizePrices()

    override def calculateOrderPrice(order: Order): Money = calculateOrderInternal(order, toppingsPrices, crustTypePrices, crustSizePrices)

    private def calculateOrderInternal(
                                        o: Order,
                                        toppingsPrices: Map[Topping, Money],
                                        crustTypePrices: Map[CrustType, Money],
                                        crustSizePrices: Map[CrustSize, Money]
                                      ) : Money = {
      val prices : Seq[Money] = for {
        pizza <- o.pizzas
      } yield calculatePizzaPrice(pizza, toppingsPrices, crustSizePrices, crustTypePrices)

      prices.sum
    }

  }

  def main(args: Array[String]) = {
    object MyPizzaService extends AbstractOrderService with PizzaService {
      object db extends PizzaComponentPricesDao
      val database = db
    }

    // MyPizzaService.calculateOrderPrice()
  }

}
