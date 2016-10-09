package pmatch

import org.scalatest.path
import pmatch.utils.TestUtils._

/**
  * Created by dnwiebe on 9/28/16.
  */
class D_TypesTest extends path.FunSpec {
  import D_Types._

  describe ("When we do it wrong with pattern matching") {
    describe ("expected types") {
      val items = List (DogWrong ("Fido"), DogWrong ("Pookie"), BlenderWrong (32.0), RifleWrong (2))

      describe ("when sounded") {
        val results = items.map (soundOf (_))

        it ("produce the expected noises") {
          assert (results === List ("woof", "yap", "whir", "crack"))
        }
      }
    }

    describe ("an unexpected type") {
      val item = CargoShipWrong (false)

      describe ("when sounded") {
        val exceptionOpt = captureException {
          soundOf (item)
        }

        it ("complains, but only at runtime") {
          assert (exceptionOpt.get.isInstanceOf [MatchError])
          assert (exceptionOpt.get.getMessage.contains ("CargoShipWrong"))
        }
      }
    }
  }

  describe ("When we do it right with polymorphism") {
    describe ("expected types") {
      val items = List (DogRight ("Fido"), DogRight ("Pookie"), BlenderRight (32.0), RifleRight (2))

      describe ("when sounded") {
        val results = items.map (_.sound)

        it ("produce the expected noises") {
          assert (results === List ("woof", "yap", "whir", "crack"))
        }
      }
    }

    describe ("an unexpected type") {
      val item = CargoShipRight (false)

      it ("cannot be sounded because of a compile-time error") {
        assertDoesNotCompile ("item.sound")
      }
    }
  }
}
