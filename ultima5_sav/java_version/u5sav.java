import java.io.RandomAccessFile;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.io.IOException;

class u5sav {
  private static final byte[] bytes9999 = new byte[] { 0x0F, 0x27  };
  private static final byte[] bytes99   = new byte[] { 0x63 };
  private static final String BASE = "C:\\GOG Games\\Ultima456\\Ultima 5\\SAVED.";
  private static final String GAM  = "GAM";
  private static final String BAK  = "GAM";

  private RandomAccessFile game;

  private void fixupHP() throws IOException {
    byte[] stats  = new byte[ 32 ];
    for( int idx = 0; idx < 16; ++idx ) {
       long location = 2 + ( idx * stats.length );
       game.seek( location );
       game.readFully( stats );
       stats[ 0x0B ] = 'G';           // in good health
       stats[ 0x10 ] = stats[ 0x12 ]; // low  byte of HP to max
       stats[ 0x11 ] = stats[ 0x13 ]; // high byte of HP to max
       game.seek( location );
       game.write( stats );
    }
  }

  private void fixupInventory() throws IOException {
    game.seek( 0x202 );
    game.write( bytes9999 );    // food
    game.write( bytes9999 );    // gold
    game.write( bytes99   );    // keys
    game.write( bytes99   );    // gems
    game.write( bytes99   );    // torches

    game.seek( 0x20B );
    game.write( bytes99 );      // skull keys

    game.seek( 0x24A );
    for( int idx = 0; idx < 64; ++idx ) {
      game.write( bytes99 );    // spells
    }
  }

  public void fixup() throws IOException { fixupHP() ; fixupInventory(); }

  public u5sav(RandomAccessFile raf) {
     game = raf;
  }

  public static void main(String[] args) {
    System.out.println( "U5 Saved Game Adjuster... java version." );
    try {
      Files.copy( Paths.get( BASE+GAM ), 
                  Paths.get( BASE+BAK ), 
                  StandardCopyOption.REPLACE_EXISTING );
      try( RandomAccessFile game = new RandomAccessFile( BASE+GAM, "rw" ) ) {
        u5sav updater = new u5sav( game );
        System.out.println( "Updating the saved game..." );
        updater.fixup();
        System.out.println( "Done!" );
      }
    } catch(IOException e) {
      System.err.println(e);
    }

  }

} 

