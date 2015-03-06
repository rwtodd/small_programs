\ vim: set filetype=forth :

: pasc cr 0 DO  1 I 1+ 0 DO 
     dup .  J I -   * I 1+   / 
   LOOP drop cr LOOP ;
16 pasc bye
