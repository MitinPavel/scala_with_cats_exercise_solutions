package chapter4

object GettingFuncy extends App {
  def buildMonad =
    new Monad[Option] {
      override def pure[Int](a: Int): Option[Int] = Some(a)

      override def flatMap[Int, String](value: Option[Int])(func: Int => Option[String]): Option[String] =
        value match {
          case Some(a) => func(a)
          case None => None
        }
    }

  println(buildMonad.map(Some(1))(_.toString.concat("!")))
}
