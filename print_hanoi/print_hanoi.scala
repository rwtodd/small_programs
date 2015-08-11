object PrintHanoi {

  def draw( d1: Array[Int], 
            d2: Array[Int], 
            d3: Array[Int], 
            spacing: Int ): Unit = {

    val height = d1.length + d2.length + d3.length
    def formatted_size( d: Int ) = d*2+1
    val colsize = formatted_size(d1.max max d2.max max d3.max) 
    val spacingstr = " "*spacing

    def drawOne( column: Array[Int], row: Int ) = { 
       val diskidx = row - (height - column.length)
       val dsz = if ( diskidx >= 0 ) formatted_size( column(diskidx) ) else 1
       val spaces = " "*((colsize - dsz)/2)
       var chars = (if(dsz == 1) "|"  else "-")*dsz
       print(s"$spaces$chars$spaces$spacingstr")
    }

    for ( row <- 0 until height ) {
       drawOne( d1, row ) ; drawOne( d2, row ) ; drawOne( d3, row )
       println
    } 
  }  

  def main( argv: Array[String] ) = 
     draw( Array(4,5,6,7), Array(2,3), Array(1), 1)
}
