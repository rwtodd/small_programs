package rwt.asciipic 

import java.awt.{Color,RenderingHints}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO


object Thumbnail {
  private def scaleImage(orig: BufferedImage, width: Int, height: Int) : BufferedImage = {
      val scaled = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
      val gfx2d = scaled.createGraphics()
      gfx2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                             RenderingHints.VALUE_INTERPOLATION_BICUBIC)
      gfx2d.drawImage(orig, 0, 0, width, height, null)
      gfx2d.dispose()                             
      scaled
  }

  // load the given image, and progressively scale it down to the given width
  def apply(fname: String, scaledWidth: Int, aspectRatio: Double) : BufferedImage = {
      var image = ImageIO.read(new File(fname))
      val scaledHeight = ((scaledWidth / aspectRatio / image.getWidth) * image.getHeight).toInt

      while(scaledWidth < image.getWidth/2) {
          image = scaleImage(image, image.getWidth/2, image.getHeight/2)
      }

      scaleImage(image, scaledWidth, scaledHeight) 
  }

}

object AsciiPic {

  // determine the brightness (0.0 to 255.0) of a Color
  private def brightness(c: Color)  = c.getRed * 0.2126 + c.getGreen * 0.7152 + c.getBlue * 0.0722 

  // select a character to use based on the given brightness
  private var chars = "#A@%$+=*:,. ".toArray
  private def selectChar(b: Double) = chars((b*chars.length/256.0).toInt)

  // convert a single line of the input from RGB to ascii
  private def doLine(im: BufferedImage, y: Int) : String = 
     (0 until im.getWidth)
          .map { x => selectChar(brightness(new Color(im.getRGB(x,y)))) }
          .mkString

  // convert an entire image from RGB to ascii
  private def convertImage(im: BufferedImage) : String =  
      (0 until im.getHeight)
          .map { y => doLine(im,y) }
          .mkString("\n")
           
  private def usage() : Unit = {
     System.err.println("Usage: asciipic [-help] [-w width] [-ar aspect-ratio] fname")
     System.err.println("   default width: 72                               ")
     System.err.println("   default aspect ratio of text (w/h): 1.5         ")
     System.exit(1)
  }

  def main(args: Array[String]) : Unit = {
    var width : Int = 72
    var ar : Double = 1.5 
    var fname = ""
    def procArgs( alist : List[String] ) : Unit = alist match { 
       case "-w" :: w :: rest =>  width = w.toInt
                                  procArgs(rest) 
       case "-ar" :: a :: rest => ar = a.toDouble
                                  procArgs(rest)
       case "-help" :: rest    => usage()
       case f :: Nil           => fname = f
       case _                  => usage()
    }
    procArgs(args.toList)
    println(convertImage(Thumbnail(fname, width, ar)))
  }
}
