object Sierp {

 private final val SZ = 64

 private def firstLine() = " "*(SZ/2) + "*" + " "*(SZ/2)

 private def nextLine(ln:String):String = {
   def at(idx:Int) = if(idx==0) { (' ',ln(1)) } 
                else if(idx == SZ-1) { (ln(SZ-2),' ') } 
                else { (ln(idx-1),ln(idx+1)) } 
   (0 until SZ).map( (x:Int) => at(x) match {
        case ('*',' ') => '*'
        case (' ','*') => '*'
        case _         => ' '
      }).mkString
 }

 def main(args:Array[String]) = {
   var curl = firstLine
   (0 until SZ/2).foreach( (idx:Int) => {
      println(curl) ;
      curl = nextLine(curl)
   } )
 }
}
