package chaprer3

object CodecInstances {
  implicit val booleanCodec: Codec[Boolean] =
    new Codec[Boolean] {
      def encode(value: Boolean): String = if (value) "yes" else "no"

      def decode(value: String): Boolean = value == "yes"
    }
}

object TransformativeThinkingWithImap extends App {

  import CodecInstances._

  println(Codec.encode(true))
  println(Codec.decode("no"))

  val intCodec = Codec.imap(
    (b: Boolean) => if (b) 1 else 0,
    (n: Int) => n == 1
  )
  println(intCodec.encode(0))
}
