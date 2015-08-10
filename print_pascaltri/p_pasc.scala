object PascalTri {
  
  def draw( height : Int ) : Unit = {

     // inner func, draw a single line of the triangle..
     def draw1( width: Int ) : Unit = {
        var cur = 1
        for ( loc <- 0 to width ) {
           print(s"$cur ")
           cur =  (width-loc)*cur / (loc+1)
        }     
        println()
     }

     // draw all the lines
     (0 until height) foreach draw1
  }

  def main( argv : Array[String] ) :Unit  = draw(16)
}
