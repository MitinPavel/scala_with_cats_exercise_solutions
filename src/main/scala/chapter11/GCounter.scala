package chapter11

final case class GCounter(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int): GCounter = {
    val current = counters.getOrElse(machine, 0)
    val incremented = current + amount
    GCounter(counters + (machine -> incremented))
  }

  def merge(that: GCounter): GCounter = {
    val machines = counters.keys ++ that.counters.keys
    val merged = machines.foldLeft(Map[String, Int]()) { (acc, machine) =>
      val count = counters.getOrElse(machine, 0).max(that.counters.getOrElse(machine, 0))
      acc + (machine -> count)
    }
    GCounter(merged)
  }

  def total: Int =
    counters.values.sum
}
