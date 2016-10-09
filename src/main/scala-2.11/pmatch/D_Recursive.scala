package pmatch

/**
  * Created by dnwiebe on 9/28/16.
  */
object D_Recursive {
  case class Tree (left: Option[Tree], right: Option[Tree], value: Int)

  /////////////////////

  def standardSum (tree: Tree): Int = {
    val leftSum = if (tree.left.isDefined) standardSum (tree.left.get) else 0
    val rightSum = if (tree.right.isDefined) standardSum (tree.right.get) else 0
    leftSum + rightSum + tree.value
  }

  /////////////////////

  def delegatingSum (tree: Tree): Int = {
    delegatingSumUtil (Some (tree))
  }

  private def delegatingSumUtil (tree: Option[Tree]): Int = {
    if (tree.isEmpty) 0 else delegatingSumUtil (tree.get.left) + delegatingSumUtil (tree.get.right) + tree.get.value
  }

  /////////////////////

  def patternMatchSum (tree: Tree): Int = {
    tree match {
      case Tree (None, None, value) => value
      case Tree (None, Some (r), value) => patternMatchSum (r) + value
      case Tree (Some (l), None, value) => patternMatchSum (l) + value
      case Tree (Some (l), Some (r), value) => patternMatchSum (l) + patternMatchSum (r) + value
    }
  }

  /////////////////////

  def delegatingPMSum (tree: Tree): Int = {
    delegatingPMSumUtil (Some (tree))
  }

  private def delegatingPMSumUtil (tree: Option[Tree]): Int = {
    tree match {
      case None => 0
      case Some (t) => delegatingPMSumUtil (t.left) + delegatingPMSumUtil (t.right) + t.value
    }
  }
}
