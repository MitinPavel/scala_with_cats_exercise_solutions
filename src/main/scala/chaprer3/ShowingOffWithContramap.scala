package chaprer3

trait Printable[A] {
  self =>

  def format(value: A): String

  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String = {
        self.format(func(value))
      }
    }
}

object PrintableInstances {
  implicit val booleanPrintable = new Printable[Boolean] {
    def format(value: Boolean) = if (value) "true" else "false"
  }
}

object PrintableSyntax {

  implicit class PrintableOps[A](value: A) {
    def contramap[B](func: B => A)(implicit p: Printable[A]): Printable[B] =
      p.contramap(func)
  }

}

object ShowingOffWithContramap extends App {

  import PrintableInstances._
  import PrintableSyntax._

  var pritnableForInt = true.contramap[Int](n => if (n == 1) true else false)
  println(pritnableForInt.format(1))
}
