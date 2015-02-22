# Hanoi Printer

This is just a silly program prompted by [yet another "challenge"][1] 
on `comp.lang.forth`.  It takes three arrays of hanoi disks, and prints
the towers.

Example:

~~~~
a[ 4 c, 5 c, 6 c, 7 c, ]a  \ tower 1
a[ 2 c, 3 c, ]a            \ tower 2
a[ 1 c, ]a                 \ tower 3
1                          \ 1 space between towers
hanoi                      \ draw it! 
~~~~

Output:

~~~~~
       |               |               |
       |               |               |
       |               |               |
   ---------           |               |
  -----------          |               |
 -------------       -----             |
---------------     -------           ---
~~~~~

[1]: https://groups.google.com/d/topic/comp.lang.forth/WazTPrGPXz0/discussion
