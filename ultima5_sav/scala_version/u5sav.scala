import java.io.RandomAccessFile

class U5Sav( val game : RandomAccessFile ) {
  private final val bytes9999 : Array[ Byte ] = Array( 0x0F, 0x27 )
  private final val bytes99   : Array[ Byte ] = Array( 0x63 )

  private def fixupHP = {
    val stats : Array[ Byte ] = new Array[ Byte ]( 32 )
    for( idx <- 0 until 16 ) { 
       val location = 2 + ( idx * stats.length )
       game.seek( location )
       game.readFully( stats )
       stats( 0x0B ) = 'G'           // in good health
       stats( 0x10 ) = stats( 0x12 ) // low  byte of HP to max
       stats( 0x11 ) = stats( 0x13 ) // high byte of HP to max
       game.seek( location )
       game.write( stats )
    }
  }

  private def fixupInventory = {
    game.seek( 0x202 )
    game.write( bytes9999 )    // food
    game.write( bytes9999 )    // gold
    game.write( bytes99   )    // keys
    game.write( bytes99   )    // gems
    game.write( bytes99   )    // torches

    game.seek( 0x20B )
    game.write( bytes99 )      // skull keys

    game.seek( 0x24A )
    for( _ <- 1 to 64 ) { 
      game.write( bytes99 )    // spells
    }
  }

  def fixup = { fixupHP ; fixupInventory }
} 

object U5Sav {
  private final val BASE = "C:\\GOG Games\\Ultima456\\Ultima 5\\SAVED."
  private final val GAM  = "GAM"
  private final val BAK  = "BAK"

  private def backup : Unit = {
    import java.nio.file.{ Files, Paths, StandardCopyOption }
    Files.copy( Paths.get( BASE+GAM ), 
                Paths.get( BASE+BAK ), 
                StandardCopyOption.REPLACE_EXISTING )
  }

  def main( args : Array[ String ] ) : Unit = {
    println( "U5 Saved Game Adjuster... scala version." )
    backup
    val game = new RandomAccessFile( BASE+GAM, "rw" ) 
    try {
      val updater = new U5Sav( game ) 
      println( "Updating the saved game..." )
      updater.fixup
      println( "Done!" )  
    } finally {
      game.close
    }
  }
}

