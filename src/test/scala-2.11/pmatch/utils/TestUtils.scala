package pmatch.utils

import java.text.SimpleDateFormat
import java.util.Date

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

  private val MMDDYYYY = new SimpleDateFormat ("MM/dd/yyyy")
  def mmddyyyy (string: String): Date = {
    MMDDYYYY.parse (string)
  }
  def mmddyyyy (date: Date): String = {
    MMDDYYYY.format (date)
  }
}
