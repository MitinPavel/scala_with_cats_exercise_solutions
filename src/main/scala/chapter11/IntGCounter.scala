package chapter11

final case class IntGCounter(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int): IntGCounter = {
    val current = counters.getOrElse(machine, 0)
    val incremented = current + amount
    IntGCounter(counters + (machine -> incremented))
  }

  def merge(that: IntGCounter): IntGCounter = {
    val machines = counters.keys ++ that.counters.keys
    val merged = machines.foldLeft(Map[String, Int]()) { (acc, machine) =>
      val count = counters.getOrElse(machine, 0).max(that.counters.getOrElse(machine, 0))
      acc + (machine -> count)
    }
    IntGCounter(merged)
  }

  def total: Int =
    counters.values.sum
}
