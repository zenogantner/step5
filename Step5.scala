import scala.io.Source
import scala.math

class User(id: Int) {}
class Item(id: Int) {}
class Rating(val user: User, val item: Item, val value: Double) {}


trait Rater {
  def rate(user: User, item: Item): Double
}


class GlobalAverage(val average: Double) extends Rater {
  def this(ratings: Seq[Rating]) = this(ratings.map(_.value).sum / ratings.length)
  def rate(user: User, item: Item) = average
}


object Tsv {
  def readRatings(filename: String): Seq[Rating] = {
    val lines = Source.fromFile(filename).getLines()
    val ratings = for (line <- lines) yield {
      val fields = line.split("\\t")
      new Rating(new User(fields(0).toInt), new Item(fields(1).toInt), fields(2).toDouble)
    }
    return ratings toList
  }
}


object Eval {
  def rmse(rater: Rater, ratings: Seq[Rating]): Double = {
    val distances = ratings map (r =>  math.pow(rater.rate(r.user, r.item) - r.value, 2))
    val sum = distances.foldLeft(0.0)(_ + _)
    return sum / distances.length
  }
}


object Run {
  def main(args: Array[String]) {
    val train = Tsv.readRatings(args(0))
    val test = Tsv.readRatings(args(1))
    val recommender = new GlobalAverage(train)
    println("#ratings: " + test.size)
    println("RMSE: " + Eval.rmse(recommender, test))
  }
}
