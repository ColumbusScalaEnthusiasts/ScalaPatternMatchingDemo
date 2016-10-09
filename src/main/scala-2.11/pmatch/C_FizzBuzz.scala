package pmatch

/**
  * Created by dnwiebe on 9/27/16.
  */
object C_FizzBuzz {

  /////////////////////

  def standardIf (number: Int): String = {
    if (number % 15 == 0) {
      "fizzbuzz"
    }
    else if (number % 3 == 0) {
      "fizz"
    }
    else if (number % 5 == 0) {
      "buzz"
    }
    else {
      number.toString
    }
  }

  /////////////////////

  def cachedIf (number: Int): String = {
    val mod3 = number % 3
    val mod5 = number % 5
    if (mod3 == 0 && mod5 == 0) {
      "fizzbuzz"
    }
    else if (mod3 == 0) {
      "fizz"
    }
    else if (mod5 == 0) {
      "buzz"
    }
    else {
      number.toString
    }
  }

  /////////////////////

  def patternMatch (number: Int): String = {
    (number % 3, number % 5) match {
      case (0, 0) => "fizzbuzz"
      case (0, _) => "fizz"
      case (_, 0) => "buzz"
      case _ => number.toString
    }
  }
}
