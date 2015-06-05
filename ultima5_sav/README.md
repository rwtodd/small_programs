Ultima 5 Cheat Program
======================

2014-01-21

[fileform]:  http://wiki.ultimacodex.com/wiki/Ultima_V_Internal_Formats
[litprog]:  http://en.wikipedia.org/wiki/Literate_programming

As much as I enjoy playing the _Ultima_ series of RPGs, I don't
particularly like the resource-management part of the game. 
Fortunately, I am a programmer!  For _Ultima V_, the details
of the [internal file formats][fileform] are pretty well-understood 
at this point, so it was no problem to make the game more to my liking.

When you run it, if gives you:

  * Full health and food
  * Plenty of the basic items (keys, gems, torches)
  * Plenty of skull keys
  * Plenty of spells and potions

It _doesn't_ give you any of the plot-driving items like the badge,
crown, or sandalwood box. It also does not give you levels, experience,
or attribute bonuses.  So, you still have to play the game, you
just don't have to worry about your next meal, etc. The code is all
very well-explained, so if you think I went overboard you can take
out whatever you'd still like to srounge around for in-game.

Multiple Versions
-----------------

I did this program in the [literate style][litprog] in `cweb`.  You
can see the woven output as well as the raw source in the repo.
I generally call it from a script that saves a backup copy of the 
`SAVED.GAM` file, just to be careful. The script 
looks like this for me:

~~~~~~ bash
#!/bin/bash
cd /cygdrive/c/GOG/Ultima456/Ultima\ 5
cp SAVED.GAM SAVED.BAK
/usr/local/bin/u5sav
~~~~~~

... but of course it will look different for you, if you choose to use
such a script at all.

I also did a version in prolog, just to see what it would be like to
do such an imperative type of program in the language.  It used a 
custom litprog tool, and I've provided the woven output for posterity.

Later, I did a scala version, which I then converted to java in order
to compare the verbosity.  It was interesting: as of June 2015 the
java code is like 300 chars longer, but the resulting class files are
about 6kb smaller for the java version (8kb vs 2kb in .class files). 

Since this is basically a script, I thought it would make sense to
make a javascript (nashorn) version from the java source. The 
translation was pretty easy.
