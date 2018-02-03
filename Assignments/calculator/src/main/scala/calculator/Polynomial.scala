package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = Signal {
    val bEntity = b()
    val aEntity = a()
    val cEntity = c()

    bEntity * bEntity - (4 * aEntity * cEntity)
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val bEntity = b()
    val aEntity = a()
//    val cEntity = c()
    val dEntity = delta()

    Set(
      (dEntity - bEntity) / 2 * aEntity,
      (-dEntity - bEntity) / 2 * aEntity
    )
  }
}
