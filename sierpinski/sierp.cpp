#include<string>
#include<iostream>

// can take any forward iterator as long as it's giving out chars...
template< typename It, typename OIt >
static void next_sierp( It first, It last, OIt out ) {
  char prev {' '};
  char cur  {*first++};
  while( first != last ) {
    char next {*first++};
    *out++ = ((prev ^ next) == 0) ? ' ' : '*'; 
    prev = cur;
    cur = next; 
  }
  *out = prev;
}

static const size_t HALF_SZ { 32 } ;

int main( int argc, char **argv ) {
  std::string sierp = std::string( HALF_SZ, ' ') + std::string(1,'*') +
                      std::string( HALF_SZ, ' '); 
  for( int idx = 0; idx < HALF_SZ; ++idx ) {
    std::cout << sierp << std::endl;
    next_sierp( begin( sierp ), end( sierp ), begin( sierp ) );
  }

  return 0;
}

