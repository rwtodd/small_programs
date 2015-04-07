# Mandelbrot Explorer

This was just a fun program, prompted by 
[a post to `comp.lang.forth`][1] asking to see a 
small ASCII mandelbrot program.

You want a big terminal window like at least 90x40 for these to look
ok... that's bigger than the windows shell default, but you can change
this in the menus. For linux of OS X it's no problem to just make the
xterm larger.

 - **mandel.fs**  just draws the ASCII picture as requested.
 - **explore.fs** lets you zoom and pan around the ASCII image. 
 - **mandel.cpp** a c++ version of the interactive program
 - **mandelbrot.scala** a scala version of the interactive program

The forth programs were tested with gforth, but may work in other
Forths without much trouble.  I'm not sure.

[1]: https://groups.google.com/d/topic/comp.lang.forth/zXQrdJOKQdY/discussion
