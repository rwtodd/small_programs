# casthex

This is a program to generate and display 
I Ching hexagrams. It can use the 3-coins 
or yarrow stalks methods, or generate a
static random hexagram with no moving lines.
You can also just enter a casting by hand
or script.

There are a few versions here, in different
programming languages.  There's also a Go
version in my [misc-go](https://github.com/rwtodd/misc-go) 
repository.


~~~~~~
$ casthex -help
Usage: casthex (-coins|-stalks|-static|<casting>)
  -coins   3-coins method
  -stalks  yarrow stalks method
  -static  a random hexagram
  <casting> 6 digits from the set {6,7,8,9}


$ casthex -coins
Casting: <888976>

45. Ts'ui -- Gathering
 - Changing to -->
20. Kuan -- Observation

  --   --   -->   -------
  -------         -------
  -------   -->   --   --
  --   --         --   --
  --   --         --   --
  --   --         --   --

~~~~~~

