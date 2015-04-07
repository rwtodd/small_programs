import scala.io.StdIn

object Mandelbrot { 
  private var scaleX = 0.04
  private var scaleY = 0.1
  private var ulX = -2.0
  private var ulY = -1.0
  private final var lenX = 72 
  private final var lenY = 20
 
  private val buf = new Array[Char](lenX) 

  private def norm(x:Double, y:Double) = x*x + y*y

  private def iterate(ptX : Double, ptY : Double):Char = { 
     var ord = '~'.toInt
     var cx = ptX
     var cy = ptY 
     while((norm(cx,cy) < 4.0) && (ord > ' '.toInt)) {
       val tmp  = cx*cy
       cx = cx*cx - cy*cy + ptX
       cy = tmp+tmp + ptY
       ord -= 1
     }
     return ord.toChar
  } 

  def scene() = {
    for(locY <- 0 until lenY) {
      val ptY = ulY + scaleY*locY
      for(locX <- 0 until lenX) {
         buf(locX) = iterate(ulX + scaleX*locX, ptY)
      }
      buf.foreach( (ch) => print(ch) )
      println()
    }
  }

  private def newPage() = println("\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")

  def main(args: Array[String]) : Unit = {
    while(true) {
      newPage()
      scene()
      StdIn.readLine foreach(_ match {
        case 'l' => ulX -= scaleX*6.0
        case 'r' => ulX += scaleX*6.0
        case 'u' => ulY -= scaleY*3.0
        case 'd' => ulY += scaleY*3.0
        case 'i' => ulX += scaleX*lenX/4 ; ulY += scaleY*lenY/4 ; 
                    scaleX /= 2 ; scaleY /= 2
        case 'o' => scaleX *= 2 ; scaleY *= 2 ;
                    ulX -= scaleX*lenX/4 ; ulY -= scaleY*lenY/4  
        case 'q' => return ()
        case _   => // nothing
      })
    }
  } 
}

