package chaprer3

trait Printable[A] {
  def format(value: A): String

  def contramap[B](func: B => A)(implicit p: Printable[A]): Printable[B] =
    new Printable[B] {
      def format(value: B): String = {
        val a: A = func(value)
        Printable.format(a)
      }
    }
}

object PrintableInstances {
  implicit val booleanPrintable = new Printable[Boolean] {
    def format(value: Boolean) = if (value) "true" else "false"
  }
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String =
    p.format(value)

  def contramap[A, B](func: B => A)(implicit p: Printable[A]): Printable[B] =
    p.contramap(func)
}

object PrintableSyntax {

  implicit class PrintableOps[A](value: A) {
    def format(implicit p: Printable[A]): String =
      p.format(value)

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
