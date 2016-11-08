package rwt.asciipic 

import java.awt.{Color,RenderingHints}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object AsciiPic {

  // load the given image, and scale it down to the given width
  def scaleImage(fname: String, width: Int) : BufferedImage = {
      val image = ImageIO.read(new File(fname))
      val scaled = new BufferedImage(width, 
                                     ((width.toDouble / image.getWidth) * 
                                         image.getHeight).toInt,
                                     BufferedImage.TYPE_INT_RGB)
      val gfx2d = scaled.createGraphics()
      gfx2d.setRenderingHint(RenderingHints.KEY_INTERPOLATION,
                             RenderingHints.VALUE_INTERPOLATION_BILINEAR)
      gfx2d.drawImage(image, 0, 0, scaled.getWidth(), scaled.getHeight, null)
      gfx2d.dispose()                             
      scaled
  }


  // determine the brightness (0.0 to 255.0) of a Color
  def brightness(c: Color)  = c.getRed * 0.2126 + c.getGreen * 0.7152 + c.getBlue * 0.0722 

  // select a character to use based on the given brightness
  val chars = "#A@%$+=*:,. ".toArray
  def selectChar(b: Double) = chars((b*chars.length/256.0).toInt)

  // convert a single line of the input from RGB to ascii
  def doLine(im: BufferedImage, y: Int) : String = 
     (0 until im.getWidth)
          .map { x => selectChar(brightness(new Color(im.getRGB(x,y)))) }
          .mkString

  // convert an entire image from RGB to ascii
  def convertImage(im: BufferedImage) : String =  
      (0 until im.getHeight)
          .map { y => doLine(im,y) }
          .mkString("\n")
           
  def main(args: Array[String]) : Unit = 
    println( args.toList match { 
              case fname :: Nil       =>  convertImage(scaleImage(fname, 72))
              case fname :: sz :: Nil =>  convertImage(scaleImage(fname, Integer.valueOf(sz)))
              case _                  => "Usage: AsciiPic fname [width]"
              })
}
