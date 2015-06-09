#include<algorithm>
#include<string>
#include<cctype>
#include<iostream>

int main( int argc, char ** argv ) {

  auto arg = std::string( (argc >= 2) ? argv[ 1 ] : "WIZARD" );

  std::cout << arg << std::endl;

  std::transform( begin( arg ), end( arg ), begin( arg ),
    [ ]( char ch ) {
        if ( std::islower( ch ) ) {
            ch = 'z' - ch + 'a' ;
        } else if ( std::isupper( ch ) ) {
            ch = 'Z' - ch + 'A' ;
        }
        return ch;
   });

  std::cout << arg << std::endl;
  return 0;
}

