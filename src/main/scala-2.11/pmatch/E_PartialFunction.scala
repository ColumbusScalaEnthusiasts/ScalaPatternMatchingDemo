package pmatch

/**
  * Created by dnwiebe on 9/29/16.
  */
object E_PartialFunction {

  val sugaredTotal: Int => Int = {n => n * n}

  val unsugaredTotal = new Function1[Int, Int] {
    override def apply (n: Int): Int = {n * n}
  }

  ///////////////////////

  val unsugaredPartial = new PartialFunction[Double, Double] {
    override def isDefinedAt (n: Double): Boolean = {
      n >= 0.0
    }

    override def apply (n: Double): Double = {
      Math.sqrt (n)
    }
  }

  ///////////////////////

  val sugaredPartial: PartialFunction[Double, Double] = {
    case n if n >= 0.0 => Math.sqrt (n)
  }
}
