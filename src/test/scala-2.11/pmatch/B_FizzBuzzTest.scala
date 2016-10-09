package pmatch

import org.scalatest.path

import pmatch.B_FizzBuzz._

/**
  * Created by dnwiebe on 9/27/16.
  */
class B_FizzBuzzTest extends path.FunSpec {

  checkMethod ("standardIf", standardIf)
  checkMethod ("cachedIf", cachedIf)
  checkMethod ("patternMatch", patternMatch)

  private def checkMethod (strategy: String, method: Int => String): Unit = {
    describe (s"For $strategy") {
      describe ("numeric inputs") {
        val result = List (1, 2, 4, 7, 8, 11, 13, 14, 16, 17, 19).map (method)

        it ("produce numeric outputs") {
          assert (result === List ("1", "2", "4", "7", "8", "11", "13", "14", "16", "17", "19"))
        }
      }

      describe ("inputs divisible by three but not five") {
        val result = List (3, 6, 9, 12, 18).map (method)

        it ("produce 'fizz'") {
          result.foreach { x => assert (x === "fizz") }
        }
      }

      describe ("inputs divisible by five but not three") {
        val result = List (5, 10, 20).map (method)

        it ("produce 'buzz'") {
          result.foreach { x => assert (x === "buzz") }
        }
      }

      describe ("inputs divisible by three and five") {
        val result = List (15, 30, 15, 60).map (method)

        it ("produce 'fizzbuzz'") {
          result.foreach { x => assert (x === "fizzbuzz") }
        }
      }
    }
  }
}
