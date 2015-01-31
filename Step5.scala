import scala.collection.mutable.HashMap
import scala.collection.mutable.Map
import scala.io.Source
import scala.math

// TODO define user and item classes/types to achieve more safety ... but for this I need to first learn how to properly implement equality for user-defined types ...

class Rating(val user: Int, val item: Int, val value: Double) {}


trait Rater {
  def rate(user: Int, item: Int): Double
}


class GlobalAverage(val average: Double) extends Rater {
  def this(ratings: Seq[Rating]) = this(ratings.map(_.value).sum / ratings.length)
  def rate(user: Int, item: Int) = average
}


// TODO this must be more elegant
object Util {
  def userAverages(ratings: Seq[Rating]): Map[Int, Double] = {
    // TODO use groupBy and mapValues etc.
    val sumByUser = new HashMap[Int, Double]()
    val countByUser = new HashMap[Int, Int]()
    for (r <- ratings) {
      sumByUser.update(r.user, sumByUser.getOrElse(r.user, 0.0) + r.value)
      countByUser.update(r.user, countByUser.getOrElse(r.user, 0) + 1)
    }
    return for ((k, v) <- sumByUser) yield (k, v / countByUser(k))
  }
}

class UserAverage(val averages: Map[Int, Double], val globalAverage: GlobalAverage) extends Rater {
  def this(ratings: Seq[Rating]) = this(Util.userAverages(ratings), new GlobalAverage(ratings))
  def rate(user: Int, item: Int) = averages.getOrElse(user, globalAverage.average)
}


object Tsv {
  def readRatings(filename: String): Seq[Rating] = {
    val lines = Source.fromFile(filename).getLines()
    val ratings = for (line <- lines) yield {
      val fields = line.split("\\t")
      new Rating(fields(0).toInt, fields(1).toInt, fields(2).toDouble)
    }
    return ratings toList
  }
}


object Eval {
  def rmse(rater: Rater, ratings: Seq[Rating]): Double = {
    val distances = ratings map (r => math.pow(rater.rate(r.user, r.item) - r.value, 2))
    val sum = distances.foldLeft(0.0)(_ + _)
    return sum / distances.length
  }
}


object Run {
  def main(args: Array[String]) {
    // read in data
    val train = Tsv.readRatings(args(0))
    println("training ratings: " + train.size)
    val test = Tsv.readRatings(args(1))
    println("test ratings: " + test.size)
    println()

    // create recommenders
    val ga = new GlobalAverage(train)
    val ua = new UserAverage(train)

    // evaluate
    for (recommender <- Vector(ga, ua)) {
      println(recommender)
      println("RMSE: " + Eval.rmse(recommender, test))
      println()
    }
  }
}
