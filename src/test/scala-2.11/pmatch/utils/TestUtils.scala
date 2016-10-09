package pmatch.utils

/**
  * Created by dnwiebe on 9/28/16.
  */
object TestUtils {

  def captureException (f: => Unit): Option[Throwable] = {
    try {
      f
      None
    }
    catch {
      case e: Throwable => Some (e)
    }
  }
}
