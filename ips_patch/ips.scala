package rwt

import java.io.RandomAccessFile
import java.io.InputStream
import java.io.FileInputStream
import java.io.BufferedInputStream
import java.nio.file.{Files,FileSystems,Path,StandardCopyOption}

// ============================================================================
// There are 3 kinds of patches.. Byte Changes, RLE Patches, and the EOF patch.
// Patches can apply themselves to random-access files, and have a useful
// toSring().
// ============================================================================
sealed trait Patch {
  def patch(tgt : RandomAccessFile) : Unit 
}

final class BytePatch(offs: Long, bs : Array[Byte]) extends Patch {
  override def toString() : String = s"Patch of length ${bs.length}"
  override def patch(tgt : RandomAccessFile) : Unit = {
     tgt.seek(offs)
     tgt.write(bs)
  }
} 

final class RlePatch(offs: Long, len: Int, value: Byte) extends Patch {
  override def toString() : String = s"RLE Patch of length ${len}, value ${value.toInt}"
  override def patch(tgt : RandomAccessFile) : Unit = {
     tgt.seek(offs)
     val buf = new Array[Byte](len)
     java.util.Arrays.fill(buf,value) 
     tgt.write(buf)
  }
}

object EOFPatch extends Patch {
  val MARKER = ('E' << 16) + ('O' << 8) + 'F'
  override def toString() : String = "End of Patches marker."
  override def patch(tgt : RandomAccessFile) : Unit = { }
}

// ============================================================================
// The IpsIterator iterates through an IPS file, producing Patches.
// ============================================================================
class IpsIterator(fname : String) extends Iterator[Patch] {
  private val src = new BufferedInputStream(new FileInputStream(fname))
  
  private val header = new String(readBuf(5), java.nio.charset.StandardCharsets.US_ASCII)
  if(!header.equals("PATCH")) throw new Exception("Not a valid IPS file.")

  private var isDone = false
  override def hasNext() : Boolean = isDone

  override def next() : Patch = {
      val offs = read3()
      if((offs == EOFPatch.MARKER) && atEOF) {
           close()
           isDone = true
           return EOFPatch
      } 

      read2() match {
          case 0 => new RlePatch(offs.toLong, read2(), read().toByte) 
          case x => new BytePatch(offs.toLong, readBuf(x))
      } 
  }

  private def read() : Int = {
     val ans = src.read()
     if (ans == -1) throw new Exception("Unexpected EOF!")
     ans
  }
  private def read2() : Int = (read() << 8) + read()
  private def read3() : Int = (read2() << 8) + read() 
  private def readBuf(size : Int) : Array[Byte] = {
     val ans = new Array[Byte](size)
     if( src.read(ans,0,size) != size ) throw new Exception("Read error!")
     ans
  }

  private def atEOF : Boolean = {
     src.mark(1)
     val ans = src.read() == -1 
     src.reset()
     ans
  }

  def close() : Unit = src.close()
}

// ============================================================================
// This is the "main" object, which parses the cmdline and runs the patch.
// ============================================================================
object Ips {

  def main(args: Array[String]) : Unit =  args.toList match { 
     case ips :: orig :: newf :: Nil => 
        Files.copy(FileSystems.getDefault().getPath(orig),
                   FileSystems.getDefault().getPath(newf),
                   StandardCopyOption.REPLACE_EXISTING) 
        val src = new IpsIterator(ips)
        val tgt = new RandomAccessFile(newf,"rw")

        try {
            src.zipWithIndex
               .foreach { case (p, idx) => 
                               println(s"Change $idx: $p") 
                               p.patch(tgt)
                        } 
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
