import scala.collection.mutable
import scala.io.Source

// TODO define user and item classes/types to achieve more safety ... but for this I need to first learn how to properly implement equality for user-defined types ...

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
  def rmse(rater: Rater, testRatings: Seq[Rating]): Double = {
    val distances = testRatings.map { r =>
      math.pow(rater.rate(r.user, r.item) - r.value, 2)
    }
    distances.sum / distances.length
  }
}


case class GlobalAverage(ratings: Seq[Rating]) extends Rater {
  lazy val average = ratings.map(_.value).sum / ratings.size

  def rate(user: Int, item: Int) = average
}


case class UserAverage(ratings: Seq[Rating], globalAverage: GlobalAverage) extends Rater {
  private lazy val averages: Map[Int, Double] = {
    val sumByUser = mutable.HashMap.empty[Int, Double]
    val countByUser = mutable.HashMap.empty[Int, Int]
    for (r <- ratings) {
      sumByUser(r.user) = sumByUser.getOrElse(r.user, 0.0) + r.value
      countByUser(r.user) = countByUser.getOrElse(r.user, 0) + 1
    }
    for ((k, v) <- sumByUser.toMap) yield k -> v / countByUser(k)
  }

  def rate(user: Int, item: Int) = averages.getOrElse(user, globalAverage.average)
}
object UserAverage {
  def apply(ratings: Seq[Rating]): UserAverage = apply(ratings, GlobalAverage(ratings))
}


object Run extends App {
  // read in data
  val train = Ratings.fromTsvFile(args(0))
  val test = Ratings.fromTsvFile(args(1))

  println(s"""training ratings: ${train.size}
             |test ratings: ${test.size}
             |""".stripMargin)

  // create raters
  val ga = GlobalAverage(train)
  val ua = UserAverage(train)

  // evaluate
  for (rater <- Seq(ga, ua)) {
    val rmse = Eval.rmse(rater, test)
    println(s"""${rater.getClass.getSimpleName}
               |RMSE: $rmse
               |""".stripMargin)
  }
}
