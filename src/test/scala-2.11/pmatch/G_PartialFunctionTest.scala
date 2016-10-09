package pmatch

import org.scalatest.path
import pmatch.utils.TestUtils._

/**
  * Created by dnwiebe on 9/28/16.
  */
class G_PartialFunctionTest extends path.FunSpec {
  import pmatch.G_PartialFunction._

  checkSquareFunction ("sugaredTotal", sugaredTotal)
  checkSquareFunction ("unsugaredTotal", unsugaredTotal)

  private def checkSquareFunction (name: String, f: Int => Int): Unit = {
    List ((-1, 1), (0, 0), (1, 1), (2, 4), (10, 100), (55, 3025)).foreach {pair =>
      val (value, expectedResult) = pair

      describe (s"$name, when used to square $value") {
        val actualResult = f (value)

        it (s"produces $expectedResult") {
          assert (actualResult === expectedResult)
        }
      }
    }
  }

  describe ("A naked partial function from Java") {
    describe ("called with a value in its domain") {
      val result = Math.sqrt (25.0)

      it ("produces the correct result") {
        assert (result === 5.0)
      }
    }

    describe ("called with a value outside its domain") {
      val result = Math.sqrt (-25.0)

      it ("returns NaN") {
        assert (result.isNaN)
      }
    }
  }

  describe ("The unsugared function") {
    describe ("called with a value in its domain") {
      val result = unsugaredPartial (25.0)

      it ("produces the correct result") {
        assert (result === 5.0)
      }
    }

    describe ("called with a value outside its domain") {
      val result = unsugaredPartial (-25.0)

      it ("returns NaN") {
        assert (result.isNaN)
      }
    }
  }

  describe ("The sugared function") {
    describe ("called with a value in its domain") {
      val result = sugaredPartial (25.0)

      it ("produces the correct result") {
        assert (result === 5.0)
      }
    }

    describe ("called with a value outside its domain") {
      val result = capture {sugaredPartial (-25.0)}

      it ("throws a MatchError") {
        assert (result.get.isInstanceOf[MatchError])
      }
    }
  }

  checkLiftedFunction ("unsugared", unsugaredPartial)
  checkLiftedFunction ("sugared", sugaredPartial)

  private def checkLiftedFunction (name: String, f: PartialFunction[Double, Double]) {
    describe (s"Lifting the $name function") {
      val lifted = f.lift
      describe ("and calling with a value in its domain") {
        val result = lifted (25.0)

        it ("produces Some (the correct result)") {
          assert (result === Some (5.0))
        }
      }

      describe ("and calling with a value outside its domain") {
        val result = lifted (-25.0)

        it ("produces None") {
          assert (result === None)
        }
      }
    }
  }
}
