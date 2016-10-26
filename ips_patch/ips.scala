package rwt

import java.io.RandomAccessFile
import java.io.InputStream
import java.io.FileInputStream
import java.io.BufferedInputStream

class IpsInput(fname : String) {
  private val src = new BufferedInputStream(new FileInputStream(fname))
  
  private val header = new String(readBuf(5), java.nio.charset.StandardCharsets.US_ASCII)
  if(!header.equals("PATCH")) throw new Exception("Not a valid IPS file.")

  def read3() : Int = {
     val b1 = src.read()
     val b2 = src.read()
     val b3 = src.read()
     if ((b1 == -1) || (b2 == -1) || (b3 == -1)) throw new Exception("Unexpected EOF!")

     (b1 << 16) + (b2 << 8) + b3 
  }

  def read2() : Int = {
     val b1 = src.read()
     val b2 = src.read()
     if ((b1 == -1) || (b2 == -1)) throw new Exception("Unexpected EOF!")

     (b1 << 8) + b2 
  }

  def read() : Byte = {
     val ans = src.read()
     if (ans == -1) throw new Exception("Unexpected EOF!")
     ans.toByte
  }

  def readBuf(size : Int) : Array[Byte] = {
     val ans = new Array[Byte](size)
     if( src.read(ans,0,size) != size ) throw new Exception("Read error!")
     ans
  }

  def atEOF : Boolean = {
     src.mark(1)
     val ans = src.read() == -1 
     src.reset()
     ans
  }

  def close() : Unit = src.close()
}

object Ips {

  val EOF = ('E' << 16) + ('O' << 8) + 'F'

  def patch(tgt : RandomAccessFile, src : IpsInput) : Unit = {
      import java.util.Arrays

      var count = 0

      while(true) {
         val offs = src.read3()
         if((offs == EOF) && src.atEOF) {
             println(s"EOF. ${count} total changes.")
             return
         } 

         count += 1

         val changeBuf = src.read2() match {
                           case 0 => val rleLen = src.read2() 
                                     val rleVal = src.read()
                                     println(s"Change ${count} (offset ${offs}): RLE of ${rleVal.toInt}, length ${rleLen}")
                                     val buf = new Array[Byte](rleLen)
                                     Arrays.fill(buf,rleVal) 
                                     buf

                           case x => println(s"Change ${count} (offset ${offs}): Patch of length ${x}")
                                     src.readBuf(x)
                        } 
         tgt.seek(offs.toLong)
         tgt.write(changeBuf)
      }

  }

  import java.nio.file.{Files,FileSystems,Path,StandardCopyOption}

  def main(args: Array[String]) : Unit =  args.toList match { 
     case ips :: orig :: newf :: Nil => 
        Files.copy(FileSystems.getDefault().getPath(orig),
                   FileSystems.getDefault().getPath(newf),
                   StandardCopyOption.REPLACE_EXISTING) 
        val src = new IpsInput(ips)
        val tgt = new RandomAccessFile(newf,"rw")

        try {
           patch(tgt, src)
        } catch {
           case x : Exception => println(x.toString)
        } finally {
           src.close()
           tgt.close()
        }
     
     case x => 
        println("Arguments should be: ipsFile origFile newFile")
  }
}
