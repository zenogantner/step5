import collection.immutable
import scala.util.Random

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
object MatrixFactorization {
  val k = 5
  val reg = 0.015
  val learnRate = 0.01
  val numIter = 10
  val rnd = new Random()

  def apply(ratings: Seq[Rating]): MatrixFactorization = train(ratings)

  def createFactors(): Array[Double] = List.fill(k)(rnd.nextGaussian * 0.1).toArray

  def updateFactors(a: Array[Double], b: Seq[Double]): Unit = {
    require(a.size == b.size)
    for (i <- List.range(0, k - 1)) {
      a(i) += learnRate * b(i)
    }
  }

  def train(ratings: Seq[Rating]): MatrixFactorization = {
    val users = ratings.map(_.user).distinct
    val items = ratings.map(_.item).distinct
    val userFactors: Map[Int, Array[Double]] = users.map(u => u -> createFactors).toMap
    val itemFactors: Map[Int, Array[Double]] = items.map(i => i -> createFactors).toMap

    val mf = new MatrixFactorization(userFactors, itemFactors,
                                     ratings.map(_.value).sum / ratings.size)

    // This is numerical computation, please excuse using imperative/non-functional stuff ;-)
    val shuffledRatings = rnd.shuffle(ratings)
    for {
      i <- 1 to numIter
      r <- shuffledRatings
      uF = userFactors(r.user)
      iF = itemFactors(r.item)
      err = r.value - mf.rate(r.user, r.item)
      userGradient = for (j <- 0 to k-1) yield err * iF(j) - reg * uF(j)
      itemGradient = for (j <- 0 to k-1) yield err * uF(j) - reg * iF(j)
    } {
      updateFactors(uF, userGradient)
      updateFactors(iF, itemGradient)
    }
    mf
  }
}


