package pmatch

import org.scalatest.path

/**
  * Created by dnwiebe on 9/28/16.
  */
class E_RecursiveTest extends path.FunSpec {

  describe ("When summing trees") {
    checkTreeSummerMethod ("Using standard recursion", E_Recursive.standardSum)
    checkTreeSummerMethod ("Using delegated recursion", E_Recursive.delegatingSum)
    checkTreeSummerMethod ("Using pattern matching", E_Recursive.patternMatchSum)
    checkTreeSummerMethod ("Using both", E_Recursive.delegatingPMSum)
  }

  private def checkTreeSummerMethod (description: String, method: E_Recursive.Tree => Int): Unit = {
    import E_Recursive.Tree

    describe (description) {
      describe ("given a single-node tree") {
        val result = method (Tree (None, None, 4))

        it ("the sum is obvious") {
          assert (result === 4)
        }
      }

      describe ("given a somewhat more complicated tree") {
        val tree = Tree (
          Some (Tree (
            Some (Tree (
              Some (Tree (None, None, 5)),
              None,
              9
            )),
            Some (Tree (
              Some (Tree (
                Some (Tree (None, None, 4)),
                Some (Tree (None, None, 7)),
                1
              )),
              None,
              8
            )),
            2
          )),
          Some (Tree (
            None,
            Some (Tree (None, None, 6)),
            7
          )),
          10
        )
        val result = method (tree)

        it ("the sum is less obvious but still right") {
          assert (result === 59)
        }
      }
    }
  }
}
