print( "Ultima V Saved Game Adjuster... Nashorn version.\n" );


var BASE = "C:\\GOG Games\\Ultima456\\Ultima 5\\SAVED.";
var GAM = "GAM";
var BAK = "BAK";


// ************************************
// Make a backup of the saved game
// ************************************
print( "Backing up " + BASE+GAM );
java.nio.file.Files.copy( java.nio.file.Paths.get( BASE+GAM ), 
                          java.nio.file.Paths.get( BASE+BAK ), 
                          java.nio.file.StandardCopyOption.REPLACE_EXISTING );


// get ready to write some bytes!
var ByteArray = Java.type( "byte[]" );
var bytes9999 = Java.to( [ 0x0F, 0x27 ], ByteArray );
var bytes99   = Java.to( [ 0x63 ],       ByteArray );

var game = new java.io.RandomAccessFile(BASE+GAM, "rw" );

// ************************************
// Fix up the player characters
// ************************************
print( "Adjusting Player Characters" );
var stats = new ByteArray( 32 );
for( idx = 0; idx < 16; ++idx ) {
    var loc = 2 + ( idx * stats.length );
    game.seek( loc );
    game.readFully( stats );
    stats[ 0x0B ] = 'G'.charCodeAt();   // in good health
    stats[ 0x10 ] = stats[ 0x12 ];      // low  byte of HP to max
    stats[ 0x11 ] = stats[ 0x13 ];      // high byte of HP to max
    game.seek( loc );
    game.write( stats );
}

// ************************************
// Fix up the inventory
// ************************************
print( "Adjusting inventory" );
game.seek( 0x202 );
game.write( bytes9999 );    // food
game.write( bytes9999 );    // gold
game.write( bytes99   );    // keys
game.write( bytes99   );    // gems
game.write( bytes99   );    // torches

game.seek( 0x20B );
game.write( bytes99 );      // skull keys

game.seek( 0x24A );
for( idx = 0; idx < 64; ++idx ) {
  game.write( bytes99 );    // spells
}


game.close();
print( "Done!" );
