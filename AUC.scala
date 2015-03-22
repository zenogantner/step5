import scala.collection.mutable

object AUC {
  def apply(rankedLabels: Seq[Double]): Double = {
    var numHits = 0
    var numPairs = 0

    val countByLabel = mutable.Map[Double, Int]()

    for (label <- rankedLabels) {
      for (smallerLabel <- countByLabel.keys.filter(_ < label))
	numHits += countByLabel(smallerLabel)
      for (otherLabel <- countByLabel.keys.filter(_ != label))
	numPairs += countByLabel(otherLabel)
      countByLabel(label) = countByLabel.getOrElse(label, 0) + 1
    }

    if (numPairs != 0) numHits.toDouble / numPairs else 0
  }

  def apply(rater: Rater, testRatings: Seq[Rating]): Double = {
    val ratingsByUser = testRatings.groupBy(_.user)
    var aucSum: Double = 0
    for ((user, ratings) <- ratingsByUser) {
      val ratedItems = ratings.map(r => (rater.rate(user, r.item), r.value))
      val originalRatingsSortedByPrediction = ratedItems.sortBy(_._1).unzip._2
      aucSum += apply(originalRatingsSortedByPrediction)
    }
    aucSum / ratingsByUser.size
  }
}
