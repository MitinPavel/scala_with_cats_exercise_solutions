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
  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
        "\"" + value + "\""
    }
  implicit val booleanPrintable: Printable[Boolean] =
    new Printable[Boolean] {
      def format(value: Boolean): String =
        if (value) "yes" else "no"
    }

  implicit def boxPrintable[A](implicit printableA: Printable[A]): Printable[Box[A]] = {
    printableA.contramap[Box[A]](_.value)
  }
}

object PrintableSyntax {

  implicit class PrintableOps[A](value: A) {
    def contramap[B](func: B => A)(implicit p: Printable[A]): Printable[B] =
      p.contramap(func)

    def format(implicit p: Printable[A]): String =
      p.format(value)
  }

}

object ShowingOffWithContramap extends App {

  import PrintableInstances._
  import PrintableSyntax._

  var pritnableForInt = true.contramap[Int](n => if (n == 1) true else false)
  println(pritnableForInt.format(1))

  println(Box(true).format)
  println(Box("hello").format)
}
