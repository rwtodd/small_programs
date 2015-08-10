object PrintHanoi {

  def draw( d1 : Array[Int], 
            d2 : Array[Int], 
            d3 : Array[Int], 
            spacing : Int ) : Unit = {

    val disks = d1.length + d2.length + d3.length
    def formatted_size( d : Int ) = d*2+1
    val colsize = formatted_size(d1.max max d2.max max d3.max) 

    def printRow(d : Int) = { 
       val dsz = formatted_size(d)
       val spaces = " "*((colsize - dsz)/2)
       var chars = (if(dsz == 1) { "|" } else { "-" })*dsz
       print(s"$spaces$chars$spaces${" "*spacing}")
    }

    def diskAtRow( dlist: Array[Int], row : Int ) = {
       val firstIndex = disks -  dlist.length
       if ( row >= firstIndex ) { dlist(row - firstIndex) } else { 0 }
    }

    for ( row <- 0 until disks ) {
       printRow( diskAtRow( d1, row ) )
       printRow( diskAtRow( d2, row ) )
       printRow( diskAtRow( d3, row ) )
       println
    } 
  }  

  def main( argv : Array[String] ) = 
     draw( Array(4,5,6,7), Array(2,3), Array(1), 1)
}
