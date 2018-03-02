package chapter2

case class Order(totalCost: Double, quantity: Double)

object OrderMonoidInstances {
  implicit val orderAddMonoid: cats.Monoid[Order] =
    new cats.Monoid[Order] {
      def combine(x: Order, y: Order): Order =
        Order(x.totalCost + y.totalCost, x.quantity + y.quantity)

      def empty = Order(0, 0)
    }
}
