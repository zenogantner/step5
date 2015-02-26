import collection.immutable
import scala.util.Random

// TODO: Look for a good linear algebra library.

case class MatrixFactorization(val userFactors: Map[Int, Array[Double]],
			       val itemFactors: Map[Int, Array[Double]],
			       val globalAverage: Double) extends Rater {
  def rate(user: Int, item: Int) = {
    // TODO cut by min and max rating
    if (userFactors.contains(user) && itemFactors.contains(item)) {
      dot(userFactors(user), itemFactors(item))
    } else {
      globalAverage
    }
  }
  def dot[T <% Double](as: Iterable[T], bs: Iterable[T]) = {
    require(as.size == bs.size)
    (for ((a, b) <- as zip bs) yield a * b).sum
  }
}


class MFTrainer {
  val k = 5
  val reg = 0.015
  val learnRate = 0.01
  val numIter = 10
  val rnd = new Random()

  def createFactors(): Array[Double] = List.fill(k)(rnd.nextGaussian * 0.1).toArray

  // TODO move somewhere else; better name?
  def update(a: Array[Double], b: Array[Double]): Unit = {
    require(a.size == b.size)
    for (i <- List.range(0, k - 1)) {
      a(i) = a(i) + learnRate * b(i)
    }
  }

  // TODO implement SGD training
  def train(ratings: Seq[Rating]): MatrixFactorization = {
    val users = ratings.map(_.user).distinct
    val items = ratings.map(_.item).distinct
    val userFactors: Map[Int, Array[Double]] = users.map(u => u -> createFactors).toMap
    val itemFactors: Map[Int, Array[Double]] = items.map(i => i -> createFactors).toMap

    val mf = new MatrixFactorization(userFactors, itemFactors,
                                     ratings.map(_.value).sum / ratings.size)

    // This is numerical computation, please excuse using imperative/non-functional stuff ;-)
    val shuffledRatings = rnd.shuffle(ratings) // TODO also try indexed access
    for (i <- 1 to numIter) {
      // TODO move into its own method
      for (r <- shuffledRatings) {
	val uF = userFactors(r.user)
	val iF = itemFactors(r.item)
	val err = r.value - mf.rate(r.user, r.item)
	val userGradient = for (j <- 0 to k-1) yield err * iF(j) - reg * uF(j)
	update(uF, userGradient.toArray)
	val itemGradient = for (j <- 0 to k-1) yield err * uF(j) - reg * iF(j)
	update(iF, itemGradient.toArray)
      }
    }
    mf
  }
}

