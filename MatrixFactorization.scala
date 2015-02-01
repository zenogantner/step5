import collection.immutable

// TODO: Look for a good linear algebra library.

case class MatrixFactorization(val userFactors: Map[Int, Array[Double]],
			       val itemFactors: Map[Int, Array[Double]]) extends Rater {
  def rate(user: Int, item: Int) = {
    // TODO handle unknown users and items
    1 + dot(userFactors(user), itemFactors(item))
  }
  def dot[T <% Double](as: Iterable[T], bs: Iterable[T]) = {
    require(as.size == bs.size)
    (for ((a, b) <- as zip bs) yield a * b).sum
  }
}


class MFTrainer {
  val k = 5

  // TODO implement SGD training
  def train(ratings: Seq[Rating]): MatrixFactorization = new MatrixFactorization(
    Map[Int, Array[Double]]().withDefaultValue(Array(0, 0, 0, 0, 0)),
    Map[Int, Array[Double]]().withDefaultValue(Array(0, 0, 0, 0, 0)))
}

