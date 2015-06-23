package scales

/** Used during typechecking to log errors. */
object Log {
  
  var errors = List[String]()
  def error(err : String) = {
    errors :+= err
  }
}
