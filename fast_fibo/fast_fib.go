package main

import (
	"fmt"
	"os"
        "math/big"
	"strconv"
)

// translated from the Haskell at:
// https://www.nayuki.io/page/fast-fibonacci-algorithms

func fibrec(n int) (*big.Int, *big.Int) {
   if n == 0 {
      return big.NewInt(0), big.NewInt(1)
   } 

   var const2 big.Int 
   const2.SetUint64(2)

   a, b := fibrec(n/2)

   // c = (b*2 - a) * a
   c := big.NewInt(0) 
   c.Mul(b,&const2)
   c.Sub(c,a)
   c.Mul(c,a)

   // d = a*a + b*b
   d := big.NewInt(0)
   var e big.Int
   e.Mul(a,a)
   d.Mul(b,b)
   d.Add(d,&e)

   if n & 1 == 0 {
     return c,d
   } else {
      c.Add(c,d)
      return d,c 
   }
}

func fib(n int) *big.Int {
   ans, _ := fibrec(n)
   return ans
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("Need a number arg.")
		return
	}
	n, _ := strconv.Atoi(os.Args[1])
	fmt.Printf("Argument was %v\n", n)

        fmt.Printf("Fib is %v\n", fib(n))
}
