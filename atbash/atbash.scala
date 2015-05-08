object Atbash {
  def atbash(ch : Char) : Char = {
      ch match {
        case it if it.isUpper => ('Z' - ch + 'A').toChar
        case it if it.isLower => ('z' - ch + 'a').toChar 
        case _                => ch
      }
  }

  def main(args : Array[String]):Unit = {
    println(s"Arg was ${args(0)}")
    println(args(0) map atbash)
  }
}

