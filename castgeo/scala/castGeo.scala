package rwt.castgeo 

object CastGeo {
  private val line = Array("*   *", "  *  ")

  private val rnd = new java.util.Random()
  private def randomFig() : Array[String] = Array.fill(4){ line(rnd.nextInt(2)) } 

  type FigRow = Array[Array[String]]   

  // display a row of geomantic figures, with initial and middle spaces
  private def disp(initSp: Int, midSp: Int, figs : FigRow) : Unit = 
     figs.transpose.foreach { row =>
         print( row.mkString(" " * initSp, " " * midSp, "\n") )
     }

  // combine adjacent geomantic figures from a row, returning the resulting row
  private def combined(figs : FigRow) : FigRow =
     figs.grouped(2).map{ pair => 
       (pair(0), pair(1)).zipped.map { (a,b) => line(if (a==b) 0 else 1) }
     }.toArray

  def main(args: Array[String]) : Unit = {
     val moms      = Array.fill(4)(randomFig)
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
