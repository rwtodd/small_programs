package com.waywardcode.math

import java.math.BigInteger

object FastFib {
   private val TWO = BigInteger.valueOf(2L)

   private def recFib(n: Int) : Tuple2[BigInteger,BigInteger] = {
	if(n == 0) {
		return (BigInteger.ZERO, BigInteger.ONE)
	}
	val (a,b) = recFib(n/2)
        val c = ((b multiply TWO) subtract a) multiply a
        val d = (a multiply a) add (b multiply b)

        (n & 1) match {
          case 0  => (c,d)
          case _  => (d, (c add d))
        }
	
   } 

   def apply(n: Int): BigInteger = recFib(n)._1
}
