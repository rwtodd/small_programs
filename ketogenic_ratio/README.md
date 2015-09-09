Ketogenic Ratio
===============

We were playing around with a ketosis-inducing diet,
and I thought it would be fun to make a calculator
for the needed ratio of carbohydrates to protein to
fat.

I [blogged a javascript calculator](http://waywardcode.com/blog/2015/0908.html),
but I also wanted to make an old-school forth version.

In the forth version, I made the interface like the ones you
see in _Thinking Forth_... very englishy. Here's an example
session:

<pre><kbd>10 carbs eaten    5.2e prot eaten   20 fat eaten</kbd>
Response: <samp>ok<samp>

<kbd>1.3e carbs eaten  12 fat eaten</kbd>
Response: <samp>ok</samp>

<kbd>.ratio</kbd>
Response: 

<samp>
You ate 11.3 Carbs, 5.2 Protein, and 32. fat.
Ketogenic Ratio:   1.78077186572277
Fat for a KR of 2: 37.4857142857143
So you need to eat 5.48571428571429 more fat.
</samp></pre>

... where `CARBS`, `PROT`, and `FAT` are selectors, 
and `EATEN` actually
adds the amounts into the right bin.  You can make additional
`EATEN` commands, and it will just add it to the 
totals. A `RESET` will start over the process.

It's a very 1960's interface, and I had fun devising it.

