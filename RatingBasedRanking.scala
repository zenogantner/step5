import scala.util.Random

object RatingBasedRanking extends App {
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
    val auc = AUC.apply(rater, test)
    println(s"""${rater.getClass.getSimpleName}
               |AUC: $auc
               |""".stripMargin)
  }
}
