#include<iostream>
#include<complex>

using value = std::complex<double> ;

inline static char mandel_point(const value &z) {
   value x {z};
   char ch = 126;

   for(; ch > 32; --ch) {
      x = x*x + z;
      if(std::norm(x) > 4.0) break;
   }
   return ch;
}

int main() {
  value upper_left { -2.0, -1.0 };
  double scalex {0.04};
  double scaley {0.1};
  const int xsz {72};
  const int ysz {20};

  while(true) {
  value coord {upper_left};
  for(int iy = 0 ; iy < ysz ; ++iy) {
     coord.imag(upper_left.imag() + iy*scaley);
     for(int ix = 0; ix < xsz; ++ix) {
        coord.real(upper_left.real() + ix*scalex);
        std::cout << mandel_point(coord) ;
     }
     std::cout << '\n'; 
  }

  char cmd;
  std::cin >> cmd ;
  switch(cmd) {
    case 'i':
       upper_left.real( upper_left.real() + xsz/4*scalex );
       upper_left.imag( upper_left.imag() + ysz/4*scaley );
       scalex /= 2; scaley /= 2;
       break; 
    case 'o':
       scalex *= 2; scaley *= 2;
       upper_left.real( upper_left.real() - xsz/4*scalex );
       upper_left.imag( upper_left.imag() - ysz/4*scaley );
       break;
    case 'l':
       upper_left.real( upper_left.real() - 6*scalex );
       break;
    case 'r':
       upper_left.real( upper_left.real() + 6*scalex );
       break;
    case 'u':
       upper_left.imag( upper_left.imag() - 6*scaley );
       break;
    case 'd':
       upper_left.imag( upper_left.imag() + 6*scaley );
       break;
    case 'q':
       return 0;
  }
  for(int xxx = 0 ; xxx < 20; ++xxx) std::cout << '\n';

  } // while
  return 0; 
}
