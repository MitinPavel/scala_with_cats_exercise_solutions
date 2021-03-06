// 1.5.5  Exercise: Equality, Liberty, and Felinity

package chapter1

object EqualityLibertyFelinity extends App {

  import cats._
  import cats.implicits._

  implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
    cat1.name === cat2.name &&
      cat1.age === cat2.age &&
      cat1.color === cat2.color
  }

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  optionCat1 === optionCat1
  optionCat1 =!= optionCat2

  optionCat1 === optionCat2
  optionCat1 =!= optionCat2
}
