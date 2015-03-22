import scala.io.Source
import scala.util.Random

// TODO define user and item classes/types to achieve more safety ... but for this I need to first learn how to properly implement equality for user-defined types ...
// TODO implement storing models (via serialization?)

case class Rating(user: Int, item: Int, value: Double)
object Rating {
  def fromTsvLine(line: String): Rating = {
    val fields = line.split("\\t")
    Rating(fields(0).toInt, fields(1).toInt, fields(2).toDouble)
  }
}


object Ratings {
  def fromTsvFile(filename: String): Seq[Rating] = {
    Source.fromFile(filename)
      .getLines
      .map(Rating.fromTsvLine)
      .toSeq
  }
}


trait Rater {
  def rate(user: Int, item: Int): Double
}


object Eval {
  /** Root mean square error */
  def rmse(rater: Rater, testRatings: Seq[Rating]): Double = {
    val distances = testRatings.map { r =>
      math.pow(rater.rate(r.user, r.item) - r.value, 2)
    }
    math.sqrt(distances.sum / distances.length)
  }
}


case class GlobalAverage(ratings: Seq[Rating]) extends Rater {
  lazy val average = ratings.map(_.value).sum / ratings.size

  def rate(user: Int, item: Int) = average
}


object AvgBy {
  def apply(ratings: Seq[Rating], selector: Function1[Rating, Int]): Map[Int, Double] = {
    ratings.groupBy(selector).mapValues((c: Seq[Rating]) => c.map(_.value).sum / c.size)
  }
}

case class UserAverage(ratings: Seq[Rating], global: GlobalAverage) extends Rater {
  private lazy val averages: Map[Int, Double] = AvgBy(ratings, _.user)

  def rate(user: Int, item: Int) = averages.getOrElse(user, global.average)
}
object UserAverage {
  def apply(ratings: Seq[Rating]): UserAverage = apply(ratings, GlobalAverage(ratings))
}

case class ItemAverage(ratings: Seq[Rating], global: GlobalAverage) extends Rater {
  private lazy val averages: Map[Int, Double] = AvgBy(ratings, _.item)

  def rate(user: Int, item: Int) = averages.getOrElse(item, global.average)
}
object ItemAverage {
  def apply(ratings: Seq[Rating]): ItemAverage = apply(ratings, GlobalAverage(ratings))
}


object RatingPrediction extends App {
  Random.setSeed(1) // Make sure we always get the same sequence of random numbers.

  // read in data
  val train = Ratings.fromTsvFile(args(0))
  val test = Ratings.fromTsvFile(args(1))

  println(s"""training ratings: ${train.size}
             |test ratings: ${test.size}
             |""".stripMargin)

  // create raters
  val ga = GlobalAverage(train)
  val ua = UserAverage(train)
  val ia = ItemAverage(train)
  val mf = MatrixFactorization(train)

  // evaluate
  for (rater <- Seq(ga, ua, ia, mf)) {
    val rmse = Eval.rmse(rater, test)
    println(s"""${rater.getClass.getSimpleName}
               |RMSE: $rmse
               |""".stripMargin)
  }
}
