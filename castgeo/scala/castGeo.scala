package rwt.castgeo 

object CastGeo {
  private val line = Array("*   *", "  *  ")

  private val rnd = new java.util.Random()
  private def randomGeo() : Array[String] = Array.fill(4){ line(rnd.nextInt(2)) } 

  // display an array of geomantic figures (a single row of them)
  private def disp(initSp: Int, midSp: Int, figs : Array[Array[String]]) : Unit = 
     figs.transpose.foreach { row =>
         print( row.mkString(" " * initSp, " " * midSp, "\n") )
     }

  // combine adjacent geomantic figures from the given Array, returning
  // an array of 1/2 size
  private def combined(figs : Array[Array[String]]) : Array[Array[String]] =
     figs.grouped(2).map{ pair => 
       (pair(0), pair(1)).zipped.map { (a,b) => line(if (a==b) 0 else 1) }
     }.toArray

  def main(args: Array[String]) : Unit = {
     val moms      = Array.fill(4)(randomGeo)
     val daughters = moms.transpose
     val line1     = (moms ++ daughters).reverse
     val nieces    = combined(line1)
     val witnesses = combined(nieces)
     val judge     = combined(witnesses)
     disp(2,5,line1)        ; println()
     disp(7,15,nieces)      ; println()
     disp(17,35,witnesses)  ; println()
     disp(37,0,judge) 
  }
}
